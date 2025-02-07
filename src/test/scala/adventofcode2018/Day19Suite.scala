package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day19.*
import munit.FunSuite

class Day19Suite extends FunSuite:
  test("determines contents of register 0 for the sample"):
    val (bind, instructions) = handleLines(importSampleLines())
    assertEquals(followInstructions(bind, instructions), 6)

  test("determines contents of register 0 for the input"):
    val (bind, instructions) = handleLines(importLines())
    assertEquals(followInstructions(bind, instructions), 1694)

  test("determines contents of register 0 for the input when register 0 start with 1"):
    val (bind, instructions) = handleLines(importLines())
    assertEquals(part2(instructions), 18964204)

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq
end Day19Suite
