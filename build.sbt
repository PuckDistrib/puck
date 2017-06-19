
import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Calendar

import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import com.typesafe.sbt.SbtNativePackager.Universal
import org.apache.commons.io.FileUtils
import sbt._
import Keys._


val classPathFileName = settingKey[String]("Location of generated classpath script")

val printClassPathFile = taskKey[File]("create a file containing the fullclass path")
val jarSources = taskKey[File]("create a jar containing source files")
val srcJarFileName = settingKey[String]("name of the jar containing source files")

def classPathFileNameTask(cfg : Configuration): Def.Initialize[Task[File]] = Def.task {
  val f = baseDirectory.value / "target" / classPathFileName.value

  val writer = new FileWriter(f)
  val fcp = (fullClasspath in cfg).value.map(_.data.absolutePath)
  writer write "#!/bin/bash\n"
  writer write fcp.mkString("export CLASSPATH=", ":", "")
  // fish style :
  //writer.write(fcp.mkString("set CLASSPATH ", ":", ""))
  writer.close()
  f
}



def commonSettings(module: String) : Seq[Setting[_]] = Seq(
  organization := "fr.lip6",
  name := s"puck-$module",
  version := "1.1",
  scalaVersion := "2.11.8",
//  sbtVersion := "0.13.11", changed by cedric
//  sbtVersion := "0.13.13", changed by cedric as it seems that sbt 0.13.13 libraries cannot be found on server: "Could not retrieve sbt 0.13.13"
  sbtVersion := "0.13.15",
  classPathFileName := "CLASSPATH",
  srcJarFileName := s"${organization.value}.${name.value}-${version.value}-src.jar",
  printClassPathFile in Test := classPathFileNameTask(Test).value,
  printClassPathFile in Compile := classPathFileNameTask(Compile).value,

  resolvers += Resolver.url("Typesafe Releases",
    url("https://repo.typesafe.com/typesafe/releases/"))(Resolver.ivyStylePatterns),

  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.1.2",
    "org.scalatest" %% "scalatest" % "2.2.1",
    "org.scala-sbt" %% "io" % "0.13.8"
  ),

  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",       // yes, this is 2 args
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-unchecked",
    "-Xlint",
    "-optimise",
    //"-Xfatal-warnings",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture",
    "-Ywarn-unused-import"  // 2.11 only
  ),

  jarSources := {

    def listFileWithRelativePaths(roots : Seq[File]) : Seq[(File, String)] =
      roots.foldLeft (Seq[(File,String)]()) {
        (acc, dir) =>
          try {
            val fs = FileUtils.listFiles(dir, Array("scala","java"), true)
            import scala.collection.JavaConversions._
            fs.toSeq.foldLeft(acc) {
              (acc0, f) =>
                acc0 :+(f, f.relativeTo(dir).get.toString)

            }
          }
          catch {
            case _ : IllegalArgumentException => sys.error(dir + " is not a valid directory")
          }
      }


    val unmanagedDirs = (unmanagedSourceDirectories in Compile).value
    val managedDirs = (managedSourceDirectories in Compile).value

    val jarFile = baseDirectory.value / "target" / srcJarFileName.value
    val dirs = unmanagedDirs ++ managedDirs filter (_.isDirectory)

    IO.jar(listFileWithRelativePaths(dirs), jarFile, new java.util.jar.Manifest())
    jarFile
  }
)


//val extendjUrl = "https://bitbucket.org/extendj/extendj.git#master"
//val extendjUrl = "https://bitbucket.org/extendj/extendj.git#9bb8c9f" //2017-04-21 first OK
val extendjUrl = "https://bitbucket.org/extendj/extendj.git#e0c932d" //2017-04-21

val extendj : ProjectReference = RootProject(uri(extendjUrl))



lazy val puckCore : Project = project.
  settings(commonSettings("core"):_*).
  settings (
    libraryDependencies +=
      "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4"
  )


lazy val puckGui =  project.
  settings(commonSettings("gui"):_*).
  settings (
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-swing" % "2.11.0-M7",
      "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4",
//      "org.piccolo2d" % "piccolo2d-core" % "3.0",
//      "org.piccolo2d" % "piccolo2d-swt" % "3.0",
//      "org.piccolo2d" % "piccolo2d-extras" % "3.0",
      "org.apache.xmlgraphics" % "xmlgraphics-commons" % "2.0.1",
      "org.apache.xmlgraphics" % "batik-util" % "1.8",
      "org.apache.xmlgraphics" % "batik-svg-dom" % "1.8",
      "org.apache.xmlgraphics" % "batik-swing" % "1.8",
      "org.apache.xmlgraphics" % "batik-svggen" % "1.8" //for begugging purposes
    )
  ).
  dependsOn (puckCore % "compile->compile")


lazy val puckJava : Project = project.
  settings (commonSettings("java"):_*).
  dependsOn (puckCore % "test->test;compile->compile")



val timeFormat = new SimpleDateFormat("yyyy-MM-dd-HHmmss")

lazy val puckExtendJ : Project = project.
  settings(commonSettings("extendj"):_*).

  settings (libraryDependencies += "org.apache.ant" % "ant" % "1.9.6").

  settings(PuckExtendJBuild.settings(extendj):_*).

  enablePlugins(JavaAppPackaging).

  dependsOn (puckJava % "test->test;compile->compile").
  dependsOn (puckGui % "test->test;compile->compile").
  settings(packageName in Universal := s"puck-distrib-${timeFormat.format(Calendar.getInstance().getTime)}" )

  //pack sources with the binaries
  //    settings (mappings in Universal ++=
  //      Seq((jarSources in puckCore).value -> ("lib/" + (srcJarFileName in puckCore).value),
  //        (jarSources in puckGui).value -> ("lib/" + (srcJarFileName in puckGui).value),
  //        jarSources.value -> ("lib/" + srcJarFileName.value)
  //
  //      ))



lazy val root = (project in file(".")
  aggregate (puckCore, puckGui, puckJava, puckExtendJ, extendj/*, puckScala*/)
  )

//  val puckScala : Project = (project.
//    settings(commonSettings("scala")_:*).
//
//    settings(
//      libraryDependencies ++= Seq(
//        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
//        //"org.scala-lang" % "scala-library" % scalaVersion.value,
//        "org.scala-lang" % "scala-reflect" % scalaVersion.value
//      )
//    ).
//
//    settings(PuckScalaBuild.settings:_*).
//
//    dependsOn (puckGui % "compile->compile")
//  )


