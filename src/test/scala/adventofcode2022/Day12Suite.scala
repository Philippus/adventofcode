package adventofcode2022

import scala.io.Source
import scala.util.Using

import adventofcode2022.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("finds fewest steps to get the best signal for the sample"):
    val map = handleLines(importSampleLines())
    assertEquals(fewestStepsToBestSignal(map), 31L)

  test("finds fewest steps to get the best signal for the input"):
    val map = handleLines(importLines())
    assertEquals(fewestStepsToBestSignal(map), 380L)

  test("finds fewest steps from elevation a to E for the sample"):
    val map = handleLines(importSampleLines())
    assertEquals(fewestStepsToBestStart(map), 29L)

  test("finds fewest steps to elevation a to E for the input"):
    val map = handleLines(importLines())
    assertEquals(fewestStepsToBestStart(map), 375L)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day12Suite
