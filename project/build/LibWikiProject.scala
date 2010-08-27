import sbt._

class LibWikiProject(info: ProjectInfo) extends DefaultProject(info) with IdeaPlugin {
    val specs = "org.scala-tools.testing" %% "specs" % "1.6.5" % "test"
}
