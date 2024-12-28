package adventofcode2022

import scala.io.Source
import scala.util.Using

import adventofcode2022.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  def importSampleLines(): Seq[String] =
    Using.resource(Source.fromResource("2022/day10sampleinput.txt")): source =>
      source.getLines().toSeq

  test("determines signal strengths for the example"):
    val instructions = importSampleLines()
    assertEquals(followInstructions(instructions), 13140)

  test("draw capital letters on the CRT"):
    val instructions = importLines()
    drawLettersOnCRT(instructions) // EHBZLRJR

  test("determines signal strengths for the input"):
    val instructions = importLines()
    assertEquals(followInstructions(instructions), 12640)
end Day10Suite
