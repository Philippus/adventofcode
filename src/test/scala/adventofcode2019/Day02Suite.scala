package adventofcode2019

import scala.io.Source
import scala.util.Using

import adventofcode2019.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("finds state for the sample input"):
    val program = importSampleLines()
    assertEquals(process(program, 0), 3500)

  test("finds state for the input"):
    val program = importLines()
    assertEquals(restoreGravityAssistProgram(program), 4090701)

  test("finds noun and verb for the input"):
    val program = importLines()
    assertEquals(findNounAndVerb(program), 6421)

  def importSampleLines(): List[Int] =
    Using.resource(
      Source.fromResource(s"2019/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList.head.split(',').toList.map(_.toInt)
end Day02Suite
