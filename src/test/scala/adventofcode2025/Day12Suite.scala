package adventofcode2025

import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Using

import adventofcode2025.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  override val munitTimeout = Duration(1, "h")

  test("calculates regions that fit their assigned presents for the sample"):
    val (presents, regions) = parse(importSampleLines())
    assertEquals(calculateRegionsThatFitPresents(presents, regions), 2)

  test("calculates upper limit of regions that fit their assigned presents for the input"):
    val (presents, regions) = parse(importLines())
    assertEquals(calculateUpperLimitOfRegionsThatFitPresents(presents, regions), 587)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2025/day12sampleinput.txt")): source =>
      source.mkString
end Day12Suite
