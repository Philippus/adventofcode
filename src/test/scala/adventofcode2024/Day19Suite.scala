package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day19.*
import munit.FunSuite

class Day19Suite extends FunSuite:
  test("counts possible designs for the sample"):
    val (patterns, designs) = handleLines(importSampleLines())
    assertEquals(countPossible(patterns, designs), 6)

  test("counts possible designs for the input"):
    val (patterns, designs) = handleLines(importLines())
    assertEquals(countPossible(patterns, designs), 240)

  test("counts possible ways for the sample"):
    val (patterns, designs) = handleLines(importSampleLines())
    assertEquals(countPossibleWays(patterns, designs), 16L)

  test("counts possible ways for the input"):
    val (patterns, designs) = handleLines(importLines())
    assertEquals(countPossibleWays(patterns, designs), 848076019766013L)

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq
end Day19Suite
