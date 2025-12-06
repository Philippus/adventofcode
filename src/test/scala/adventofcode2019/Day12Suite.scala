package adventofcode2019

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2019.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("finds cycles when moons are back to initial state for the sample"):
    val input = importSampleLines()
    val moons = parse(input)
    assertEquals(findCyclesOfMoons(moons, 0, moons, List.empty[Moon], (0, 0, 0)), BigInt(2772))

  test("finds cycles when moons are back to initial state for the input"):
    val input = importLines()
    val moons = parse(input)
    assertEquals(findCyclesOfMoons(moons, 0, moons, List.empty[Moon], (0, 0, 0)), BigInt(551272644867044L))

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
