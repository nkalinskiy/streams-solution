package streams

import cats.Monad
import cats.data.Validated.Invalid
import cats.effect.kernel.Concurrent
import cats.effect.std.Console
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monad._
import fs2._
import fs2.io.file.{Files, Path}
import fs2.text.utf8
import streams.Validator.ValidationResult

object Stages {
  private def hash(word: String, const: Int): Int =
    word
      .map(_.toInt)
      .fold(0)((hash, charCode) => ((hash << 5) ^ (hash >> 27)) ^ charCode) % const

  private def toWords[F[_]]: Pipe[F, String, String] = stream =>
    stream
      .through(text.lines)
      .filter(_.trim.nonEmpty)
      .map(_.replaceAll("[^a-zA-Z0-9 ]", ""))
      .flatMap(line => Stream.emits(line.split(" ")))
      .filter(_.trim.nonEmpty)

  private def minHash[F[_]: Concurrent](seed: Int, concurrencyLimit: Int = 10): Pipe[F, String, Int] = stream =>
    stream
      .mapAsyncUnordered(concurrencyLimit)(word => hash(word, seed).pure[F])
      .fold(Int.MaxValue)(_ min _)

  def minHashInFile[F[_]: Files: Concurrent](source: Path, seed: Int, concurrencyLimit: Int = 10): F[Unit] = {
    val ext = source.extName
    val outPath = Path(source.toString.replace(ext, "_out" + ext))

    Files[F]
      .readAll(source)
      .through(utf8.decode)
      .through(toWords)
      .through(minHash[F](seed, concurrencyLimit))
      .map(_.toString)
      .through(utf8.encode)
      .through(Files[F].writeAll(outPath))
      .compile
      .drain
  }

  def readFromConsoleUntilValid[F[_]: Console: Monad, A](
    hint: String
  )(validation: String => F[ValidationResult[A]]): F[A] = {
    val readAndValidate =
      Console[F].println(hint) >>
        Console[F].readLine
          .flatMap(validation)
          .flatTap {
            case Invalid(errors) => Console[F].println(s"Invalid input: ${errors.toString}. Please, retry")
            case _               => ().pure[F]
          }

    readAndValidate.iterateUntil(_.isValid).map(_.toOption.get)
  }
}
