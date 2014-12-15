lazy val root = (project in file(".")).
  settings(
    name := "linked_data",
    version := "1.0",
    scalaVersion := "2.11.4",
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.0.1"
    )
  )