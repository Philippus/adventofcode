package adventofcode2023

import scala.collection.immutable.Seq

import adventofcode2023.Day11.*
import munit.FunSuite

class Day11Suite extends FunSuite:
  val exampleGrid = Array(
    "...#......".toCharArray,
    ".......#..".toCharArray,
    "#.........".toCharArray,
    "..........".toCharArray,
    "......#...".toCharArray,
    ".#........".toCharArray,
    ".........#".toCharArray,
    "..........".toCharArray,
    ".......#..".toCharArray,
    "#...#.....".toCharArray
  )

  test("finds galaxies"):
    assertEquals(
      findGalaxies(exampleGrid),
      Seq(Pos(0, 3), Pos(1, 7), Pos(2, 0), Pos(4, 6), Pos(5, 1), Pos(6, 9), Pos(8, 7), Pos(9, 0), Pos(9, 4))
    )

  test("generates combinations of galaxies"):
    assertEquals(generateCombinations(findGalaxies(exampleGrid)).size, 36)

  test("calculates distance between galaxies"):
    assertEquals(calculateDistance(Seq((Pos(6, 1), Pos(11, 5)))), 9)
    assertEquals(
      calculateDistance(generateCombinations(
        findGalaxies(transposeMatrix(expandUniverse(transposeMatrix(expandUniverse(exampleGrid)))))
      )),
      374
    )

    test("transposes matrix"):
      val matrix           = Array(
        "#...".toCharArray,
        ".#..".toCharArray,
        "..#.".toCharArray,
        "...#".toCharArray,
        "#..#".toCharArray
      )
      val transposedMatrix = transposeMatrix(matrix)
      transposedMatrix.foreach(line => println(line.mkString))
      assertEquals(transposeMatrix(matrix), matrix)

  test("calculates distance between galaxies for the input"):
    importLines()
    assertEquals(calculateDistance(generateCombinations(findGalaxies(grid))), 9222626)
    assertEquals(
      calculateDistance(
        generateCombinations(findGalaxies(transposeMatrix(expandUniverse(transposeMatrix(expandUniverse(grid))))))
      ),
      9521550
    )

  test("calculates distance between galaxies for the older universe"):
    assertEquals(calculateOlderUniverse(), BigInt(298932923702L))
end Day11Suite
