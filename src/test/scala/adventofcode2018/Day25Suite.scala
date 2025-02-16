package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day25.*
import munit.FunSuite

class Day25Suite extends FunSuite:
  test("counts constellations for the sample"):
    val fixedPoints = handleLines(importSampleLines())
    assertEquals(constellations(fixedPoints), 2)

  test("counts constellations for the input"):
    val fixedPoints = handleLines(importLines())
    assertEquals(constellations(fixedPoints), 396)

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toVector
end Day25Suite
