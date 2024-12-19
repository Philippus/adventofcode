package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("draws the map for the sample"):
    val trailheads = handleLines(importSampleLines())
    val str        = drawGrid(trailheads, 7, 7)
    println(s"$str")

  test("sums scores of trailheads for the sample"):
    assertEquals(sumScoreOfTrailheads(handleLines(importSampleLines())), 36)

  test("sums scores of trailheads for the input"):
    assertEquals(sumScoreOfTrailheads(handleLines(importLines())), 754)

  test("sums ratings of trailheads for the sample"):
    assertEquals(sumScoreOfTrailheadsPt2(handleLines(importSampleLines())), 81)

  test("sums ratings of trailheads for the input"):
    assertEquals(sumScoreOfTrailheadsPt2(handleLines(importLines())), 1609)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day10Suite
