package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day18.*
import munit.FunSuite

class Day18Suite extends FunSuite:
  test("minimum steps to reach the exit for the sample"):
    val coordinates = handleLines(importSampleLines())
    assertEquals(minimumStepsToExit(coordinates, Pos(6, 6), 12), 22L)

  test("minimum steps to reach the exit for the input"):
    val coordinates = handleLines(importLines())
    assertEquals(minimumStepsToExit(coordinates, Pos(70, 70), 1024), 302L)

  test("finds first fallen byte blocking exit for the sample"):
    val coordinates = handleLines(importSampleLines())
    assertEquals(findFirstFallenByteBlockingExit(coordinates, Pos(6, 6)), "6,1")

  test("finds first fallen byte blocking exit for the input"):
    val coordinates = handleLines(importLines())
    assertEquals(findFirstFallenByteBlockingExit(coordinates, Pos(70, 70)), "24,32")

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toVector
end Day18Suite
