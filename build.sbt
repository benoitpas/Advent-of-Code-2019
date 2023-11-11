lazy val root = project
  .in(file("."))
  .settings(

    scalaVersion := "3.3.1",

    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test"
  )
