organization := "org.goldenport"

name := "goldenport-sexpr"

version := "2.1.0-SNAPSHOT"

scalaVersion := "2.12.7"

crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.1.0-SNAPSHOT"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
