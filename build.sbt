lazy val circeV     = "0.14.3"
lazy val shapelessV = "2.3.10"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / name                 := "shapeless-circe-patched-type-keytag"
ThisBuild / organization         := "com.github.dmytromitin"
ThisBuild / organizationName     := "Dmytro Mitin"
ThisBuild / organizationHomepage := Some(url("https://github.com/DmytroMitin"))
ThisBuild / version              := "0.1"
ThisBuild / scalaVersion         := "2.13.10"
ThisBuild / scmInfo := Some(ScmInfo(
  url("https://github.com/DmytroMitin/shapeless-circe-patched-type-keytag"),
  "https://github.com/DmytroMitin/shapeless-circe-patched-type-keytag.git"
))
ThisBuild / developers := List(Developer(
  id = "DmytroMitin",
  name = "Dmytro Mitin",
  email = "dmitin3@gmail.com",
  url = url("https://github.com/DmytroMitin")
))
ThisBuild / description := "Patched Scala-3/Dotty compiler and Eval library"
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/DmytroMitin/shapeless-circe-patched-type-keytag"))
// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credential")
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / sonatypeCredentialHost := "oss.sonatype.org"
ThisBuild / sonatypeRepository := "https://oss.sonatype.org/service/local"
//ThisBuild / publishTo := {
//  val nexus = "https://oss.sonatype.org/"
//  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
//  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
//}

lazy val root = (project in file("."))
  .aggregate(
    core,
    circeGeneric,
    shapeless,
  )
  .settings(
    crossScalaVersions := Nil,
    publish / skip := true,
  )

lazy val core = project
  .settings(
    name := "core",
    resolvers ++= Resolver.sonatypeOssRepos("releases"),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core"   % circeV,
//      "io.circe" %% "circe-generic" % circeV exclude("com.chuusai", "shapeless_2.13"),
      "io.circe" %% "circe-parser" % circeV,
      "com.github.dmytromitin" %% "circe-generic-patched-type-keytag" % circeV,
      "com.github.dmytromitin" %% "shapeless-patched-type-keytag"     % shapelessV,
    ),
    scalacOptions ++= Seq(
      "-Ymacro-debug-lite",
    ),
    publish / skip := true,
  )
//  .dependsOn(
////    shapeless,
//    circeGeneric,
//  )

//https://repo1.maven.org/maven2/io/circe/circe-generic_2.13/0.14.3/
lazy val circeGeneric = project
  .settings(
    name := "circe-generic-patched-type-keytag",
    version := circeV,
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeV,
    )
  )
  .dependsOn(shapeless)

//https://repo1.maven.org/maven2/com/chuusai/shapeless_2.13/2.3.10/
lazy val shapeless = project
  .settings(
    name := "shapeless-patched-type-keytag",
    version := shapelessV,
    libraryDependencies ++= Seq(
      scalaOrganization.value % "scala-reflect"  % scalaVersion.value,
      scalaOrganization.value % "scala-compiler" % scalaVersion.value,
    ),
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "main" / "managed",
  )