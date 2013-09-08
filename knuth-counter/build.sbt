name := "knuthcounter"

scalaVersion := "2.10.2"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
