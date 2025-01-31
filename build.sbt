name         := "adventofcode2023"
organization := "nl.gn0s1s"
startYear    := Some(2023)

developers := List(
  Developer(
    id = "philippus",
    name = "Philippus Baalman",
    email = "",
    url = url("https://github.com/philippus")
  )
)

scalaVersion := "3.6.3"

mainClass.withRank(KeyRanks.Invisible) := Some("adventofcode2024.day1.Day1")

libraryDependencies ++= Seq(
  "org.scalameta"    %% "munit"                     % "1.0.4" % Test,
  "org.scalameta"    %% "munit-scalacheck"          % "1.0.0" % Test,
  "io.spray"         %% "spray-json"                % "1.3.6",
  "nl.gn0s1s"        %% "between"                   % "0.6.0",
  "org.apache.pekko" %% "pekko-actor-typed"         % "1.1.3",
  "org.apache.pekko" %% "pekko-actor-testkit-typed" % "1.1.3" % Test,
  "ch.qos.logback"    % "logback-classic"           % "1.5.16"
)
