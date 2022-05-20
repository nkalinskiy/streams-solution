import sbt._

object Dependencies {
  val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11"

  val catsEffect = "org.typelevel" %% "cats-effect" % "3.3.11"

  object fs2 {
    private val version = "3.2.7"
    val core = "co.fs2" %% "fs2-core" % version
    val io = "co.fs2" %% "fs2-io" % version
  }
}
