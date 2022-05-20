package streams

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.applicative._
import streams.Stages._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val validator = Validator[IO]()

    for {
      path <- readFromConsoleUntilValid("Enter input file path:")(validator.validateFile)
      seed <- readFromConsoleUntilValid("Enter hash seed:")(validator.validateSeed(_).pure[IO])
      _ <- minHashInFile[IO](path, seed)
    } yield ExitCode.Success
  }
}
