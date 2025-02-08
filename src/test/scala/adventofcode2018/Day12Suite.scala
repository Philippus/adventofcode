package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("grows the pots for the sample"):
    val (pots, notes) = handleLines(importSampleLines())
    assertEquals(grow(pots, notes, 20), 325L)

  test("grows the pots for the input"):
    val (pots, notes) = handleLines(importLines())
    assertEquals(grow(pots, notes, 20), 3276L)

  test("grows the pots for the input"):
    val (pots, notes) = handleLines(importLines())
    assertEquals(potsAfter50000000000Generations(pots, notes), 3750000001113L)

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toVector
end Day12Suite
