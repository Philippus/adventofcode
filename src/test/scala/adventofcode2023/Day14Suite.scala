package adventofcode2023

import scala.io.Source
import scala.util.Using

import adventofcode2023.Day14.*
import munit.FunSuite

class Day14Suite extends FunSuite:
  test("calculates total load after tilting north for the sample"):
    val platform    = parse(importSampleLines())
    val tiltedNorth = tiltNorth(platform)
    assertEquals(calculateTotalLoad(tiltedNorth), 136)

  test("calculates total load after tilting north for the input"):
    val platform    = parse(importLines())
    val tiltedNorth = tiltNorth(platform)
    assertEquals(calculateTotalLoad(tiltedNorth), 107951)

  test("calculates total load after 1000000000 cycles for the sample"):
    val platform = parse(importSampleLines())
    assertEquals(cycle1000000000Times(platform), 64)

  test("calculates total load after 1000000000 cycles for the input"):
    val platform = parse(importLines())
    assertEquals(cycle1000000000Times(platform), 95736)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2023/day14sampleinput.txt")): source =>
      source.mkString
end Day14Suite
