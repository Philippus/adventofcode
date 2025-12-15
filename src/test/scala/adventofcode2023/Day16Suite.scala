package adventofcode2023

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2023.Day16.*
import munit.FunSuite

class Day16Suite extends FunSuite:
  override val munitTimeout = Duration(300, "s")

  test("calculates energized tiles from top-left heading right for the sample"):
    val grid = parse(importSampleLines())
    assertEquals(energizedTilesFromTopLeftCornerHeadingRight(grid), 46)

  test("calculates energized tiles from top-left heading right for the input"):
    val grid = parse(importLines())
    assertEquals(energizedTilesFromTopLeftCornerHeadingRight(grid), 6740)

  test("finds configuration energizing largest number of tiles for the sample"):
    val grid = parse(importSampleLines())
    assertEquals(findConfigurationEnergizingLargestNumberOfTiles(grid), 51)

  test("finds configuration energizing largest number of tiles for the input"):
    val grid = parse(importLines())
    assertEquals(findConfigurationEnergizingLargestNumberOfTiles(grid), 7041)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2023/day16sampleinput.txt")): source =>
      source.mkString
end Day16Suite
