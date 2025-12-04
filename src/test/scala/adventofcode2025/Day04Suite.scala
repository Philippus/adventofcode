package adventofcode2025

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2025.Day04.*
import munit.FunSuite

class Day04Suite extends FunSuite:
  test("counts accessible rolls for the sample"):
    val input = importSampleLines()
    val grid  = parse(input)
    println(drawGrid(grid))
    val count = countAccessibleRolls(grid)
    println(drawGrid(grid))
    assertEquals(count, 13)

  test("counts accessible rolls for the input"):
    val input = importLines()
    val grid  = parse(input)
    assertEquals(countAccessibleRollsAndUpdateGrid(grid), 1384)

  test("counts removable rolls for the sample"):
    val input = importSampleLines()
    val grid  = parse(input)
    val count = countRemovableRolls(grid)
    assertEquals(count, 43)

  test("counts removable rolls for the input"):
    val input = importLines()
    val grid  = parse(input)
    assertEquals(countRemovableRolls(grid), 8013)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2025/day04sampleinput.txt")): source =>
      source.mkString
end Day04Suite
