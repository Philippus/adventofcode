package adventofcode2024

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2024.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  override val munitTimeout = Duration(500, "s")

  test("draws the map for the sample"):
    val (obstacles, guard) = handleLines(importSampleLines())
    val str                = drawGrid(obstacles, guard, 10, 10)
    println(s"$str")

  test("counts distinct positions of the guard for the sample"):
    val (obstacles, guard) = handleLines(importSampleLines())
    assertEquals(countDistinctPositions(obstacles, guard, 10, 10), 41)

  test("counts distinct positions of the guard for the input"):
    val (obstacles, guard) = handleLines(importLines())
    assertEquals(countDistinctPositions(obstacles, guard, 130, 130), 5095)

  test("counts possible obstructions for the sample"):
    val (obstacles, guard) = handleLines(importSampleLines())
    assertEquals(countObstructionsThatCauseLoop(obstacles, guard, 10, 10), 6)

  test("counts possible obstructions for the input"):
    val (obstacles, guard) = handleLines(importLines())
    assertEquals(countObstructionsThatCauseLoop(obstacles, guard, 130, 130), 1933)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day06Suite
