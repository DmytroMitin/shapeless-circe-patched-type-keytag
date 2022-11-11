ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val circeV = "0.14.3"

lazy val core = project
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core"   % circeV,
//      "io.circe" %% "circe-generic" % circeV exclude("com.chuusai", "shapeless_2.13"),
      "io.circe" %% "circe-parser" % circeV,
    ),
    scalacOptions ++= Seq(
      "-Ymacro-debug-lite",
    )
  )
  .dependsOn(
//    shapeless,
    circeGeneric,
  )

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
    version := "2.3.10",
    libraryDependencies ++= Seq(
      scalaOrganization.value % "scala-reflect"  % scalaVersion.value,
      scalaOrganization.value % "scala-compiler" % scalaVersion.value,
    ),
    Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "main" / "managed",
  )