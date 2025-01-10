package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("counts joltage differences for the sample"):
    val adapters = importSampleLines()
    assertEquals(countJoltageDifferences(adapters), 35)

  test("counts joltage differences for the input"):
    val adapters = importLines()
    assertEquals(countJoltageDifferences(adapters), 2277)

  test("counts arrangements of adapters for the sample"):
    val adapters = importSampleLines()
    assertEquals(countArrangements(adapters), 8L)

  test("counts arrangements of adapters for the input"):
    val adapters = importLines()
    assertEquals(countArrangements(adapters), 37024595836928L)

  def importSampleLines(): List[Int] =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList.map(_.toInt)
end Day10Suite
