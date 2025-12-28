package adventofcode2021

import scala.io.Source
import scala.util.Using

import adventofcode2021.Day17.*
import munit.FunSuite

class Day17Suite extends FunSuite:
  test("finds velocity values within target area with highest y position for the sample"):
    val targetArea = parse(importSampleLines())
    assertEquals(velocityValuesWithinTargetAreaWithHighestYpos(targetArea), 45)

  test("finds velocity values within target area with highest y position for the input"):
    val targetArea = parse(importLines())
    assertEquals(velocityValuesWithinTargetAreaWithHighestYpos(targetArea), 7503)

  test("finds distinct velocity values within target area for the sample"):
    val targetArea = parse(importSampleLines())
    assertEquals(velocityValuesWithinTargetArea(targetArea), 112)

  test("finds distinct velocity values within target area for the input"):
    val targetArea = parse(importLines())
    assertEquals(velocityValuesWithinTargetArea(targetArea), 3229)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2021/day17sampleinput.txt")): source =>
      source.mkString
end Day17Suite
