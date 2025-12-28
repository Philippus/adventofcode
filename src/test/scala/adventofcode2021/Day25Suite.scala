package adventofcode2021

import scala.io.Source
import scala.util.Using

import adventofcode2021.Day25.*
import munit.FunSuite

class Day25Suite extends FunSuite:
  test("finds first step on which no sea cucumbers move for the sample"):
    val grid = parse(importSampleLines())
    assertEquals(findStepOnWhichNoSeaCucumbersMove(grid), 58)

  test("finds first step on which no sea cucumbers move for the input"):
    val grid = parse(importLines())
    assertEquals(findStepOnWhichNoSeaCucumbersMove(grid), 563)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2021/day25sampleinput.txt")): source =>
      source.mkString
end Day25Suite
