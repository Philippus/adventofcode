package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day13.*
import munit.FunSuite

class Day13Suite extends FunSuite:
  test("detects first crash position for the sample"):
    val (map, carts) = handleLines(importSampleLines())
    assertEquals(moveCarts(map, carts), Pos(7, 3))

  test("detects first crash position for the input"):
    val (map, carts) = handleLines(importLines())
    assertEquals(moveCarts(map, carts), Pos(100, 21))

  test("finds final location for the sample"):
    val (map, carts) = handleLines(importSampleLinesForPartTwo())
    assertEquals(moveCarts(map, carts, removeCarts = true), Pos(6, 4))

  test("finds final location for the input"):
    val (map, carts) = handleLines(importLines())
    assertEquals(moveCarts(map, carts, removeCarts = true), Pos(113, 109))

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): lines =>
      lines.getLines().toVector

  def importSampleLinesForPartTwo(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput-pt2.txt")
    ): lines =>
      lines.getLines().toVector
end Day13Suite
