package adventofcode2025

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2025.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("calculates splits for the sample"):
    val input = importSampleLines()
    assertEquals(calculateSplits(parse(input)), 21)

  test("calculates splits for the input"):
    val input = importLines()
    assertEquals(calculateSplits(parse(input)), 1651)

  test("calculates timelines for the sample"):
    val input = importSampleLines()
    assertEquals(calculateTimelines(parse(input)), 40L)

  test("calculates timelines for the input"):
    val input = importLines()
    assertEquals(calculateTimelines(parse(input)), 108924003331749L)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2025/day07sampleinput.txt")): source =>
      source.mkString
end Day07Suite
