package adventofcode2023.day2

import scala.io.Source
import scala.util.Using

object Day2 {
  def possibleGame(line: String, red: Int, green: Int, blue: Int): Int = {
    var possible = true
    val Array(game, allRounds) = line.split(":").map(_.trim)
    val rounds = allRounds.split(";").map(_.trim)
    rounds.map { round =>  // 1 red, 2 green, 6 blue
      val colors = round.split(",").map(_.trim)
      colors.map { color =>
        val Array(count, c) = color.split(" ")
        (count, c) match {
          case (c, "red") if c.toInt > red => possible = false
          case (c, "green") if c.toInt > green => possible = false
          case (c, "blue") if c.toInt > blue => possible = false
          case _ =>
        }
      }
    }
    if (possible)
      game.split(" ").last.toInt
    else
      0
  }

  def powerGame(line: String, red: Int, green: Int, blue: Int): Int = {
    var minRed = 0
    var minGreen = 0
    var minBlue = 0
    val Array(_, allRounds) = line.split(":").map(_.trim) // Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    val rounds = allRounds.split(";").map(_.trim)
    rounds.map { round => // 1 red, 2 green, 6 blue
      val colors = round.split(",").map(_.trim)
      colors.map { color =>
        val Array(count, c) = color.split(" ")
        (count, c) match {
          case (c, "red") if c.toInt > minRed => minRed = c.toInt
          case (c, "green") if c.toInt > minGreen => minGreen = c.toInt
          case (c, "blue") if c.toInt > minBlue => minBlue = c.toInt
          case _ =>
        }
      }
    }
    minRed * minBlue * minGreen
  }

  def readInputDocument(calibrationFunction: (String, Int, Int, Int) => Int, red: Int, green: Int, blue: Int): Int = {
    Using.resource(Source.fromResource("day2input.txt")) {
      _.getLines().map(line => calibrationFunction(line, red, green, blue)).sum
    }
  }
}
