package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day16.*
import munit.FunSuite

class Day16Suite extends FunSuite:
  test("draws the map for the sample"):
    val map = handleLines(importSampleLines())
    val str = drawGrid(map, 15, 15)
    println(s"$str")

  test("finds lowest score for reindeer for the sample"):
    val map = handleLines(importSampleLines())
    assertEquals(lowestScoreForReindeer(map), 7036L)

  test("finds lowest score for reindeer for the second sample"):
    val map = handleLines(importSampleLines2())
    assertEquals(lowestScoreForReindeer(map), 11048L)

  test("finds lowest score for reindeer for a sample containing going west"):
    val map = handleLines(importSampleLines3())
    assertEquals(lowestScoreForReindeer(map), 4013L)

  test("finds lowest score for reindeer for the input"):
    val map = handleLines(importLines())
    assertEquals(lowestScoreForReindeer(map), 135512L)

  def importSampleLines3(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput3.txt")
    ): source =>
      source.getLines().toList

  def importSampleLines2(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput2.txt")
    ): source =>
      source.getLines().toList

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day16Suite
