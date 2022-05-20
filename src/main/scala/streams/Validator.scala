package streams

import cats.Monad
import cats.data.ValidatedNel
import cats.implicits._
import fs2.io.file.{Files, Path}
import streams.Validator.ValidationResult

import scala.util.{Failure, Success, Try}

trait Validator[F[_]] {

  def validateSeed(seed: String): ValidationResult[Int]

  def validateFile(filePath: String): F[ValidationResult[Path]]
}

object Validator {
  type ValidationResult[A] = ValidatedNel[InputError, A]

  val SupportedExtensions = Set(".txt")

  def apply[F[_]: Files: Monad](supportedExtensions: Set[String] = SupportedExtensions): Validator[F] =
    new Impl[F](supportedExtensions)

  private class Impl[F[_]: Files: Monad](supportedExtension: Set[String]) extends Validator[F] {
    override def validateSeed(seed: String): ValidationResult[Int] =
      nonEmptyInput(seed, "seed").andThen(validateSeedInt)

    override def validateFile(filePath: String): F[ValidationResult[Path]] =
      validateFilePath(filePath).fold(
        errors => errors.invalid[Path].pure[F],
        path => validateFileExists(path).map(_.as(path))
      )

    private def validateFilePath(filePath: String): ValidationResult[Path] =
      nonEmptyInput(filePath, "path").andThen(validateExtension)

    private def validateFileExists(path: Path): F[ValidationResult[Unit]] =
      Monad[F].ifF(Files[F].exists(path))(
        ().validNel[InputError],
        FileDoesNotExist(path.toString).invalidNel[Unit]
      )

    private def validateSeedInt(intStr: String): ValidationResult[Int] =
      Try(intStr.toInt) match {
        case Failure(_)     => InvalidHashConst(intStr).invalidNel
        case Success(value) => if (value > 0) value.validNel else InvalidHashConst(intStr).invalidNel
      }

    private def nonEmptyInput(in: String, paramName: String): ValidationResult[String] =
      if (in.nonEmpty) in.validNel else EmptyInput(paramName).invalidNel

    private def validateExtension(nonEmptyString: String): ValidationResult[Path] = {
      val path = Path(nonEmptyString)
      val ext = path.extName
      if (supportedExtension contains ext) path.validNel else InvalidExtension(ext).invalidNel
    }
  }
}
