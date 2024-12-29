package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  test("finds first wrong number for the sample"):
    val numbers = importSampleLines()
    assertEquals(findFirstWrongNumber(numbers, 5), 127L)

  test("finds first wrong number for the input"):
    val numbers = importLines()
    assertEquals(findFirstWrongNumber(numbers, 25), 2089807806L)

  def importSampleLines(): List[Long] =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList.map(_.toLong)
end Day09Suite
