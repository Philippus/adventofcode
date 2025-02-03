package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day20.*
import munit.FunSuite

class Day20Suite extends FunSuite:
  test("finds fastest time for the sample"):
    assertEquals(picosecondsTroughRacetrack(handleLines(importSampleLines()), Pos(-100, -100), Pos(-200, -200)), 84L)

  test("finds cheats saving time for the sample"):
    assertEquals(cheatsThatSaveMoreThanNPicoseconds(handleLines(importSampleLines()), 38), 3L)

  test("finds cheats saving time for the input"):
    assertEquals(cheatsThatSaveMoreThanNPicoseconds(handleLines(importSampleLines()), 100), 1502L)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day20Suite
