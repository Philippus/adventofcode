package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day17.*
import munit.FunSuite

class Day17Suite extends FunSuite:
  test("runs programs for the example"):
    assertEquals(runProgram(Seq(2, 6), 0, 0, 9)._2, 1)
    assertEquals(runProgram(Seq(5, 0, 5, 1, 5, 4), 10, 0, 0)._4, Seq(0, 1, 2))
    assertEquals(runProgram(Seq(0, 1, 5, 4, 3, 0), 2024, 0, 0)._1, 0)
    assertEquals(runProgram(Seq(0, 1, 5, 4, 3, 0), 2024, 0, 0)._4, Seq(4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0))
    assertEquals(runProgram(Seq(1, 7), 0, 29, 0)._2, 26)
    assertEquals(runProgram(Seq(4, 0), 0, 2024, 43690)._2, 44354)
    assertEquals(runProgram(Seq(0, 1, 5, 4, 3, 0), 729, 0, 0)._4, Seq(4, 6, 3, 5, 6, 3, 5, 2, 1, 0))

  test("runs program for the input"):
    val init = initialize(importLines())
    assertEquals(runProgram(init._4, init._1, init._2, init._3)._4.mkString(","), "1,4,6,1,6,4,3,0,3")

  test("finds quine for the sample"):
    assertEquals(findQuine(Seq(0, 3, 5, 4, 3, 0)), 117440)

  test("finds quine for the input"):
    assertEquals(findQuineAdv(initialize(importLines())._4), 265061364597659L)

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq
end Day17Suite
