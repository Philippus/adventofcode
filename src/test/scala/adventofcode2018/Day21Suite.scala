package adventofcode2018

import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Using

import adventofcode2018.Day21.*
import munit.FunSuite

class Day21Suite extends FunSuite:
  override val munitTimeout = Duration(500, "s")

  test(
    "finds lowest non-negative integer value for register 0 after executing the fewest instructions that causes the program to halt"
  ):
    val (bind, instructions) = handleLines(importLines())
    assertEquals(followInstructions(bind, instructions, stopAtFirst = true), 6132825L)

  test(
    "finds lowest non-negative integer value for register 0 after executing most instructions that causes the program to halt"
  ):
    val (bind, instructions) = handleLines(importLines())
    assertEquals(followInstructions(bind, instructions), 8307757L)

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq
end Day21Suite
