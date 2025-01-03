package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day15.*
import munit.FunSuite

class Day15Suite extends FunSuite:
  test("finds 2020th spoken number for the sample"):
    val startingNumbers = importSampleLines()
    assertEquals(nthSpokenNumber(startingNumbers, 2020), 436L)

  test("finds 2020th spoken number for the input"):
    val startingNumbers = importLines()
    assertEquals(nthSpokenNumber(startingNumbers, 2020), 620L)

  test("finds 30000000th spoken number for the sample"):
    val startingNumbers = importSampleLines()
    assertEquals(nthSpokenNumber(startingNumbers, 30000000), 175594L)

  test("finds 30000000th spoken number for the input"):
    val startingNumbers = importLines()
    assertEquals(nthSpokenNumber(startingNumbers, 30000000), 110871L)

  def importSampleLines(): List[Long] =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().next().split(',').map(_.toLong).toList
end Day15Suite
