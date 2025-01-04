package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("draws the map for the sample"):
    val points = handleLines(importSampleLines())
    val str    = drawGrid(points)
    println(s"$str")

  test("finds the message for the sample"):
    val points = handleLines(importSampleLines())
    println(drawGrid(cohesiveMessage(points, 10)._1))

  test("finds the message for the input"):
    val points = handleLines(importLines())
    println(drawGrid(cohesiveMessage(points, 70)._1)) // RPNNXFZR

  test("finds seconds until message for the sample"):
    val points = handleLines(importSampleLines())
    assertEquals(cohesiveMessage(points, 10)._2, 3)

  test("finds seconds until message for the input"):
    val points = handleLines(importLines())
    assertEquals(cohesiveMessage(points, 70)._2, 10946)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day10Suite
