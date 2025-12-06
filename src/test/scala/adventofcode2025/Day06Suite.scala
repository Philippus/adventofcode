package adventofcode2025

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2025.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("calculates grand total for the sample"):
    val input = importSampleLines()
    val lines = input.split('\n').toList
    assertEquals(grandTotal(lines), 4277556L)

  test("calculates grand total for the input"):
    val input = importLines()
    val lines = input.split('\n').toList
    assertEquals(grandTotal(lines), 3261038365331L)

  test("calculates grand total from right to left for the sample"):
    val input = importSampleLines()
    val lines = input.split('\n').toList
    assertEquals(rightToLeftGrandTotal(lines), 3263827L)

  test("calculates grand total from right to left for the input"):
    val input = importLines()
    val lines = input.split('\n').toList
    assertEquals(rightToLeftGrandTotal(lines), 8342588849093L)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2025/day06sampleinput.txt")): source =>
      source.mkString
end Day06Suite
