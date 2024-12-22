package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day15.*
import munit.FunSuite

class Day15Suite extends FunSuite:
  test("draws the warehouse for the tiny sample"):
    val str = drawWarehouse(handleLines(importTinySampleLines())._1, 8, 8)
    println(str)

  test("detects blockades"):
    val (things, moves) = handleLines(importTinySampleLines())
    val robot           = things.find(_._2 == '@').get
    assert(isBlocked('<', things, robot._1._1, robot._1._2))

  test("walks the robot for the tiny sample"):
    val (things, moves) = handleLines(importTinySampleLines())
    val str             = drawWarehouse(walkRobot(things, moves), 8, 8)
    println(str)

  test("walks the robot for the sample"):
    val (things, moves) = handleLines(importSampleLines())
    val str             = drawWarehouse(walkRobot(things, moves), 10, 10)
    println(str)

  test("calculates sum of all boxes' GPS coordinates for the tiny sample"):
    val (things, moves) = handleLines(importTinySampleLines())
    assertEquals(sumAllBoxesGPSCoordinates(things, moves), 2028)

  test("calculates sum of all boxes' GPS coordinates for the sample"):
    val (things, moves) = handleLines(importSampleLines())
    assertEquals(sumAllBoxesGPSCoordinates(things, moves), 10092)

  test("calculates sum of all boxes' GPS coordinates for the input"):
    val (things, moves) = handleLines(importLines())
    assertEquals(sumAllBoxesGPSCoordinates(things, moves), 1456590)

  def importTinySampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}tinysampleinput.txt")
    ): source =>
      source.getLines().toList

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day15Suite
