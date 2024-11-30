package adventofcode2021

import adventofcode2021.Day05.*
import munit.FunSuite

class Day05Suite extends FunSuite:
  test("determines positions"):
    val line = Line((0, 9), (2, 9))
    assertEquals(positions(line), Seq((0, 9), (1, 9), (2, 9)))

  test("finds overlapping points for the example"):
    val lines =
      "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
        .split(
          '\n'
        ).map(handleLine).toSeq
    assertEquals(overlappingPoints(lines), 5)

  test("finds overlapping points for the input"):
    val lines = importLines().map(handleLine)
    assertEquals(overlappingPoints(lines), 8111)

  test("determines positions"):
    val line = Line((3, 3), (1, 1))
    assertEquals(diagonalPositions(line), Seq((3, 3), (2, 2), (1, 1)))

  test("finds overlapping points including diagonals for the input"):
    val lines = importLines().map(handleLine)
    assertEquals(overlappingPointsIncludingDiagonals(lines), 22088)
end Day05Suite
