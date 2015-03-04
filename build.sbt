organization := "org.goldenport"

name := "goldenport-sexpr"

version := "1.1.1"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

// libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

// libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3" % "compile"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.1.0"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
