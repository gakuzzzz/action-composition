import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

  val appName         = "action-composition"
  val appVersion      = "1.0"

  val appDependencies = Seq(
    "commons-codec" % "commons-codec" % "1.5"
  )

  val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
    // Add your own project settings here
  )

}
