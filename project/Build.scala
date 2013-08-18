import sbt._
import Keys._
import Project.Setting

object ScalaTourBuild extends Build {

  lazy val root = Project(
    id = "scala-tour",
    base = file("."),
    settings = Project.defaultSettings ++ buildSettings) dependsOn(knockoffProject)

  lazy val knockoffProject = RootProject(uri(
    "git://github.com/tristanjuricek/knockoff.git"))

  def buildSettings: Seq[Setting[_]] = Seq(
    name := "scala-tour",
    version := "1.0",
    libraryDependencies ++= Seq( ),
    resolvers ++= Seq( )
  )
}