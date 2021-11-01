organization := "org.goldenport"

name := "goldenport-sexpr"

version := "2.0.22"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-feature"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Maven" at "https://repo1.maven.org/maven2"

resolvers += "GitHab releases 2019" at "https://raw.github.com/asami/maven-repository/2019/releases"

resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2021/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.3.5"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "1.3.43" % "provided"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % "compile"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "compile"

libraryDependencies += "commons-jxpath" % "commons-jxpath" % "1.3" % "compile"

libraryDependencies += "org.apache.commons" % "commons-jexl3" % "3.0"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.13.2" % "compile"

// libraryDependencies += "org.scalanlp" %% "breeze-viz" % "0.13.2" % "compile"

// libraryDependencies += "org.apache.camel" % "camel-core" % "3.0.0-M1" % "compile"

libraryDependencies += "org.apache.camel" % "camel-core" % "2.23.1" % "compile"

libraryDependencies += "com.amazonaws" % "aws-java-sdk" % "1.11.519" % "compile"

libraryDependencies += "com.amazonaws" % "aws-java-sdk" % "1.11.519" % "compile"

libraryDependencies += "org.apache.spark" %% "spark-core" % "2.2.3" % "compile" exclude("org.glassfish.hk2", "hk2-utils") exclude("org.glassfish.hk2", "hk2-locator") exclude("javax.validation", "validation-api") exclude("org.slf4j", "slf4j-log4j12") // Use old version for Scala 2.10

libraryDependencies += "org.apache.spark" %% "spark-sql" % "2.2.3" % "compile" exclude("org.glassfish.hk2", "hk2-utils") exclude("org.glassfish.hk2", "hk2-locator") exclude("javax.validation", "validation-api") exclude("org.slf4j", "slf4j-log4j12") // Use old version for Scala 2.10

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

//
val mavenrepo = settingKey[String]("mavenrepo")

mavenrepo := sys.env.getOrElse("PUBLISH_MAVEN_REPO", default = "target/maven-repository")

publishTo <<= mavenrepo { v: String =>
  Some(Resolver.file("file", file(v)))
}
