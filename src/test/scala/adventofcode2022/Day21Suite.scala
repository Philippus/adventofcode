package adventofcode2022

import scala.io.Source
import scala.util.Using

import adventofcode2022.Day21.*
import munit.FunSuite

class Day21Suite extends FunSuite:
  test("finds number that monkey named root will yell for the sample"):
    assertEquals(findRoot(handleLines(importSampleLines())), 152L)

  test("finds number that monkey named root will yell for the input"):
    assertEquals(findRoot(handleLines(importLines())), 256997859093114L)

  test("finds number that humn should yell for the sample"):
    assertEquals(findHumn(handleLines(importSampleLines())), 301L)

  test("finds number that humn should yell for the input"):
    assertEquals(findHumn(handleLines(importLines())), 3952288690726L)

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toVector
end Day21Suite
