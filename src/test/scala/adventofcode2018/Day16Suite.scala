package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day16.*
import munit.FunSuite

class Day16Suite extends FunSuite:
  test("determines matching opcodes for the sample"):
    val sample = Sample(Vector(3, 2, 1, 1), Vector(9, 2, 1, 2), Vector(3, 2, 2, 1))
    assertEquals(testSample(sample).toSet, Set("mulr", "addi", "seti"))

  test("determines samples behaving like three or more opcodes for the input"):
    val samples = handleLines(importLines())._1
    assertEquals(samplesBehavingLikeThreeOrMoreOpcodes(samples), 560)

  test("determines opcodes and executes test program"):
    val (samples, instructions) = handleLines(importLines())
    assertEquals(determineOpcodesAndExecuteTestProgram(samples, instructions), 622)

  def importSampleLines(): (Int, Int) =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().next() match
        case s"$n players; last marble is worth $ps points" => (n.toInt, ps.toInt)
end Day16Suite
