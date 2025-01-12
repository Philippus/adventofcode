package adventofcode2017

import scala.io.Source
import scala.util.Using

import adventofcode2017.Day19.*
import munit.FunSuite

class Day19Suite extends FunSuite:
  test("finds letters along path for the sample"):
    val map = handleLines(importSampleLines())
    assertEquals(followPath(map)._1, "ABCDEF")

  test("finds letters along path for the input"):
    val map = handleLines(importLines())
    assertEquals(followPath(map)._1, "XYFDJNRCQA")

  test("calculates steps of path for the sample"):
    val map = handleLines(importSampleLines())
    assertEquals(followPath(map)._2, 38)

  test("calculates steps of path for the input"):
    val map = handleLines(importLines())
    assertEquals(followPath(map)._2, 17450)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day19Suite
