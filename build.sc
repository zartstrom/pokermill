// build.sc
import mill._
import mill.scalajslib.ScalaJSModule
import scalalib._

trait CommonModule {
  def scalaVersion = "2.12.7"
}

object EvalHand extends ScalaModule {
  def scalaVersion = "2.12.7"
  def circeVersion = "0.10.0"
  // def forkArgs = Seq("-Xprof")
  override def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:1.4.0",
    ivy"io.circe::circe-core:${circeVersion}",
    ivy"io.circe::circe-generic:${circeVersion}",
    ivy"io.circe::circe-parser:${circeVersion}",
    ivy"com.lihaoyi::fastparse:2.0.4",
    ivy"com.lihaoyi::upickle:0.6.7",
    ivy"net.debasishg::redisclient:3.8"
    // ivy"com.timgroup::iterata::0.1.7"
  )

  object test extends Tests {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks   = Seq("org.scalatest.tools.Framework")
  }
}

object Preflop extends ScalaModule {
  def scalaVersion        = "2.12.7"
  override def moduleDeps = Seq(EvalHand)

  // def scalaJSVersion = "0.6.25"
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.0.4"
  )

  object test extends Tests {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.5")
    def testFrameworks   = Seq("org.scalatest.tools.Framework")
  }
}

object Web extends ScalaModule {
  def scalaVersion        = "2.12.7"
  override def moduleDeps = Seq(EvalHand, Preflop)

  // def scalaJSVersion = "0.6.25"
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::cask:0.1.9",
    ivy"com.lihaoyi::fastparse:2.0.4",
    ivy"com.lihaoyi::scalatags:0.6.7",
    ivy"org.xerial:sqlite-jdbc:3.18.0",
    ivy"io.getquill::quill-jdbc:2.5.4"
  )

  object test extends Tests{
    def testFrameworks = Seq("utest.runner.Framework")

    override def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.6.3",
      ivy"com.lihaoyi::requests::0.1.5",
    )
  }
}
