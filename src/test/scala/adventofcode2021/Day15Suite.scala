package adventofcode2021

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2021.Day15.*
import munit.FunSuite

class Day15Suite extends FunSuite:
  override val munitTimeout = Duration(1, "h")

  test("calculates lowest risk for the sample"):
    val lines = importSampleLines()
    val map   = handleLines(lines)
    assertEquals(lowestRisk(map), 40L)

  test("calculates lowest risk for the input"):
    val lines = importLines()
    val map   = handleLines(lines)
    assertEquals(lowestRisk(map), 403L)

  test("calculates lowest risk for the expanded map of the sample"):
    val lines = importSampleLines()
    val map   = expandMap(handleLines(lines))
    assertEquals(lowestRisk(map), 315L)

  test("calculates lowest risk for the expanded map of the input"):
    val lines = importLines()
    val map   = expandMap(handleLines(lines))
    assertEquals(lowestRisk(map), 2840L)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2021/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day15Suite
