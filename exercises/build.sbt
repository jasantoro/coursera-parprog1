name := "exercirses"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-deprecation")

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % "test"
libraryDependencies += "com.storm-enroute" %% "scalameter-core" % "0.6"

