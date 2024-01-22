package adventofcode2020

import adventofcode2020.Day05.*
import munit.FunSuite

class Day05Suite extends FunSuite:
  test("determines row"):
    assertEquals(determineRow("FBFBBFFRLR"), 44)

  test("determines column"):
    assertEquals(determineColumn("FBFBBFFRLR"), 5)

  test("calculates seat id"):
    assertEquals(calculateSeatId("BFFFBBFRRR"), 567)
    assertEquals(calculateSeatId("FFFBBBFRRR"), 119)
    assertEquals(calculateSeatId("BBFFBBFRLL"), 820)

  test("calculates max seat id for the input file"):
    val boardingPasses = readInputFile()
    assertEquals(boardingPasses.map(calculateSeatId).max, 832)

  test("finds my seat id"):
    val seatIds = readInputFile().map(calculateSeatId)
    assertEquals(findMySeatId(seatIds), 517)
end Day05Suite
