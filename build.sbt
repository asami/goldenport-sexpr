organization := "org.goldenport"

name := "goldenport-sexpr"

version := "2.0.1"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.2.33"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.2.35" % "provided"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % "provided"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "provided"

libraryDependencies += "commons-jxpath" % "commons-jxpath" % "1.3" % "provided"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2" % "provided"

libraryDependencies += "org.scalanlp" %% "breeze-viz" % "0.13.2" % "provided"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
