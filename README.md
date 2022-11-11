# shapeless-circe-patched-type-keytag
https://stackoverflow.com/questions/74384745/encoding-decoding-a-field-with-any-datatype-assigned-with-value-none-in-scala

https://github.com/milessabin/shapeless

https://repo1.maven.org/maven2/com/chuusai/shapeless_2.13/2.3.10/

https://oss.sonatype.org/content/groups/public/com/github/dmytromitin/shapeless-patched-type-keytag_2.13/2.3.10/

https://github.com/circe/circe

https://repo1.maven.org/maven2/io/circe/circe-generic_2.13/0.14.3/

https://oss.sonatype.org/content/groups/public/com/github/dmytromitin/circe-generic-patched-type-keytag_2.13/0.14.3/

```scala
scalaVersion := "2.13.10"
resolvers ++= Resolver.sonatypeOssRepos("releases")
libraryDependencies ++= Seq(
  "io.circe" %% "circe-core"   % "0.14.3",
  "io.circe" %% "circe-parser" % "0.14.3",
  "com.github.dmytromitin" %% "circe-generic-patched-type-keytag" % "0.14.3",
  "com.github.dmytromitin" %% "shapeless-patched-type-keytag"     % "2.3.10"
)
scalacOptions += "-Ymacro-debug-lite"
```

```scala
sbt clean compile core/run
```