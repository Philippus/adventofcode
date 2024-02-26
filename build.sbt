name := "adventofcode2023"
organization := "nl.gn0s1s"
startYear := Some(2023)

developers := List(
  Developer(
    id = "philippus",
    name = "Philippus Baalman",
    email = "",
    url = url("https://github.com/philippus")
  )
)

scalaVersion := "3.4.0"

mainClass.withRank(KeyRanks.Invisible)   := Some("adventofcode2023.day1.Day1")

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "0.7.29" % Test,
  "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
  "io.spray" %%  "spray-json" % "1.3.6"
)
