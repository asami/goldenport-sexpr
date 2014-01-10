organization := "org.goldenport"

name := "goldenport-sexpr"

version := "0.2.4"

// scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
