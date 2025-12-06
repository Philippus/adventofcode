package adventofcode2019

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2019.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("calculates energy of the system for the sample"):
    val input = importSampleLines()
    assertEquals(calculateEnergy(parse(input), 10), 179)

  test("calculates energy of the system for the input"):
    val input = importLines()
    assertEquals(calculateEnergy(parse(input), 1000), 10845)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2019/day12sampleinput.txt")): source =>
      source.mkString
end Day12Suite
