package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day11.*
import munit.FunSuite

class Day11Suite extends FunSuite:
  test("counts stones for the samples"):
    assertEquals(countStones(importSampleLines(), 1), 7L)
    assertEquals(countStones(Seq(125L, 17L), 6), 22L)
    assertEquals(countStones(Seq(125L, 17L), 25), 55312L)

  test("counts stones for the input part one"):
    assertEquals(countStones(importLines(), 25), 203228L)

  test("counts stones for the input part two"):
    assertEquals(countStones(importLines(), 75), 240884656550923L)

  def importSampleLines(): Seq[Long] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().take(1).flatMap(_.split(' ').map(_.toLong)).toSeq
end Day11Suite
