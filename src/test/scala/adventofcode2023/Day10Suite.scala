package adventofcode2023

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

import adventofcode2023.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("finds starting position"):
    val grid = Array(
      ".....".toCharArray.map(c => (c, -1)),
      ".S-7.".toCharArray.map(c => (c, -1)),
      ".|.|.".toCharArray.map(c => (c, -1)),
      ".L-J.".toCharArray.map(c => (c, -1)),
      ".....".toCharArray.map(c => (c, -1))
    )
    assertEquals(findStartPos(grid), Pos(1, 1))

  test("finds next pos"):
    val grid = Array(
      ".....".toCharArray.map(c => (c, -1)),
      ".S-7.".toCharArray.map(c => (c, -1)),
      ".|.|.".toCharArray.map(c => (c, -1)),
      ".L-J.".toCharArray.map(c => (c, -1)),
      ".....".toCharArray.map(c => (c, -1))
    )
    grid(1)(1) = ('F', 0)

    assertEquals(nextPos(grid, Pos(1, 1)), Some(Pos(2, 1)))

  test("finds max distance in the big grid"):
    assertEquals(findMaxInBigGrid(), 6956)
end Day10Suite
