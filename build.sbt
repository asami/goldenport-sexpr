organization := "org.goldenport"

name := "goldenport-sexpr"

version := "2.0.1"
// version := "1.1.7"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
