name := "Hashcats"

version := "Awesome"

scalaVersion := "2.10.3"

// set the main Scala source directory to be <base>/src
scalaSource in Compile := baseDirectory.value / "src"

// unmanagedSourceDirectories in Compile += baseDirectory.value / "libs"