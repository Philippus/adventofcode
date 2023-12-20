package adventofcode2017

import adventofcode2017.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("calculates checksum of rows"):
    val rows = Seq(
      "5 1 9 5",
      "7 5 3",
      "2 4 6 8"
    )
    assertEquals(rowsChecksum(rows), 18)
    assertEquals(rowsChecksum(readInputFile), 50376)

  test("calculates evenly divisible checksum of rows"):
    val rows = Seq(
      "5 9 2 8",
      "9 4 7 3",
      "3 8 6 5"
    )
    assertEquals(evenlyDivisibleChecksum(rows), 9)
    assertEquals(evenlyDivisibleChecksum(readInputFile), 267)
end Day02Suite
