package adventofcode2016

import scala.io.Source
import scala.util.Using

import adventofcode2016.Day24.*
import munit.FunSuite

class Day24Suite extends FunSuite:
  test("finds shortest route from start to goal"):
    val map = handleLines(importSampleLines())
    assertEquals(fewestStepsToGoal(map, '0', '4'), 2L)
    assertEquals(fewestStepsToGoal(map, '4', '1'), 4L)
    assertEquals(fewestStepsToGoal(map, '1', '2'), 6L)
    assertEquals(fewestStepsToGoal(map, '2', '3'), 2L)

  test("finds shortest route from start, visiting all non-0 numbers for the sample"):
    val map = handleLines(importSampleLines())
    assertEquals(fewestStepsVisitingAllNonZeroNumbers(map), 14L)

  test("finds shortest route from start, visiting all non-0 numbers for the input"):
    val map = handleLines(importLines())
    assertEquals(fewestStepsVisitingAllNonZeroNumbers(map), 464L)

  test("finds shortest route from start, visiting all non-0 numbers returning to 0 for the input"):
    val map = handleLines(importLines())
    assertEquals(fewestStepsVisitingAllNonZeroNumbers(map, returnToZero = true), 652L)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2016/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day24Suite
