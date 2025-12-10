package adventofcode2025

import scala.io.Source
import scala.util.Using

import adventofcode2025.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("calculates fewest button presses for the sample"):
    val input    = importSampleLines()
    val machines = parse(input)
    assertEquals(fewestButtonPresses(machines), 7L)

  test("calculates fewest button presses for the input"):
    val input    = importLines()
    val machines = parse(input)
    assertEquals(fewestButtonPresses(machines), 449L)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2025/day10sampleinput.txt")): source =>
      source.mkString
end Day10Suite
