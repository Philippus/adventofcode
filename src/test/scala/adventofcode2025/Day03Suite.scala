package adventofcode2025

import scala.io.Source
import scala.util.Using

import adventofcode2025.Day03.*
import munit.FunSuite

class Day03Suite extends FunSuite:
  test("calculates maximum joltage for the sample"):
    assertEquals(maximumJoltageWith2Batteries(importSampleLines()), 357)

  test("calculates maximum joltage for the input"):
    assertEquals(maximumJoltageWith2Batteries(importLines()), 17535)

  test("calculates maximum joltage for the input using generic method"):
    assertEquals(maximumJoltageWithNBatteries(importLines(), 2), 17535L)

  test("calculates maximum joltage with 12 batteries per bank for the sample"):
    assertEquals(maximumJoltageWith12Batteries(importSampleLines()), 3121910778619L)

  test("calculates maximum joltage with 12 batteries per bank for the input"):
    assertEquals(maximumJoltageWith12Batteries(importLines()), 173577199527257L)

  def importSampleLines(): List[String] =
    Using.resource(Source.fromResource("2025/day03sampleinput.txt")): source =>
      source.getLines().toList
end Day03Suite
