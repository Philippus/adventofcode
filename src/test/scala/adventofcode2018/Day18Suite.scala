package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day18.*
import munit.FunSuite

class Day18Suite extends FunSuite:
  test("determines resource value after 10 minutes for the sample"):
    val map = handleLines(importSampleLines())
    assertEquals(resourceValue(map, 10), 1147)

  test("determines resource value after 10 minutes for the input"):
    val map = handleLines(importLines())
    assertEquals(resourceValue(map, 10), 384480)

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toVector
end Day18Suite
