// build.sc
import mill._, scalalib._

object ticket4Sale extends ScalaModule {
  def scalaVersion = "2.13.6"
  override def ammoniteVersion = "2.4.0"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle:1.3.8",
    ivy"io.github.zamblauskas::scala-csv-parser:0.13.1"
  )

  object test extends Tests {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.10")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}
