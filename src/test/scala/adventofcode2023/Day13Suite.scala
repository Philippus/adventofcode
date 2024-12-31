package adventofcode2023

import scala.io.Source
import scala.util.Using

import adventofcode2023.Day13.*
import munit.FunSuite

class Day13Suite extends FunSuite:
  test("finds reflections for the sample"):
    val lines = importSampleLines()
    assertEquals(findReflections(lines), 405)

  test("finds reflections for the input"):
    val lines = importLines()
    assertEquals(findReflections(lines), 37381)

  test("finds reflections despite smudges for the sample"):
    val lines = importSampleLines()
    assertEquals(findReflectionsDespiteSmudges(lines), 400)

  test("finds reflections despite smudges for the input"):
    val lines = importLines()
    assertEquals(findReflectionsDespiteSmudges(lines), 28210)

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2023/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().mkString("\n").split("\n\n").toVector
end Day13Suite
