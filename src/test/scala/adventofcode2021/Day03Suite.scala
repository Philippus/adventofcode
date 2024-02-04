package adventofcode2021

import adventofcode2021.Day03.*
import munit.FunSuite

class Day03Suite extends FunSuite:
  test("calculates gamma rate for the example"):
    val numbers = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010".split('\n').toSeq
    assertEquals(gammaRate(numbers), "10110")

  test("calculates epsilon rate for the example"):
    val numbers = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010".split('\n').toSeq
    assertEquals(epsilonRate(numbers), "01001")

  test("calculates power consumption for the example"):
    val numbers = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010".split('\n').toSeq
    assertEquals(powerConsumption(gammaRate(numbers), epsilonRate(numbers)), 198)

  test("calculates power consumption for the input"):
    val numbers = importLines()
    assertEquals(powerConsumption(gammaRate(numbers), epsilonRate(numbers)), 2724524)

  test("calculates oxygen generator rating for the example"):
    val numbers = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010".split('\n').toSeq
    assertEquals(oxygenGeneratorRating(numbers), "10111")

  test("calculates co2 scrubber rating for the example"):
    val numbers = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010".split('\n').toSeq
    assertEquals(co2ScrubberRating(numbers), "01010")

  test("calculates life support rating for the example"):
    val numbers = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010".split('\n').toSeq
    assertEquals(lifeSupportRating(oxygenGeneratorRating(numbers), co2ScrubberRating(numbers)), 230)

  test("calculates life support rating for the input"):
    val numbers = importLines()
    assertEquals(lifeSupportRating(oxygenGeneratorRating(numbers), co2ScrubberRating(numbers)), 2775870)
end Day03Suite
