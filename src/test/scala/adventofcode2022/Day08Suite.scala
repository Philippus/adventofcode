package adventofcode2022

import adventofcode2022.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("determines visibility"):
    val map: Vector[Vector[Int]] =
      handleLines(Seq(
        "30373",
        "25512",
        "65332",
        "33549",
        "35390"
      ).map(_.map(_.asDigit)))
    assert(isVisible(0, 0, map))
    assert(isVisible(0, 1, map))
    assert(isVisible(0, 2, map))
    assert(isVisible(0, 3, map))
    assert(isVisible(0, 4, map))
    assert(isVisible(1, 0, map))
    assert(isVisible(2, 0, map))
    assert(isVisible(3, 0, map))
    assert(isVisible(4, 0, map))
    assert(isVisible(4, 1, map))
    assert(isVisible(4, 2, map))
    assert(isVisible(4, 3, map))
    assert(isVisible(4, 4, map))
    assert(isVisible(1, 4, map))
    assert(isVisible(2, 4, map))
    assert(isVisible(3, 4, map))
    assert(isVisible(4, 4, map))
    assert(!isVisible(3, 1, map))

  test("counts visible trees for the example"):
    val map: Vector[Vector[Int]] =
      handleLines(Seq(
        "30373",
        "25512",
        "65332",
        "33549",
        "35390"
      ).map(_.map(_.asDigit)))
    assertEquals(visibleTrees(map), 21)

  test("counts visible trees for the input"):
    val map = handleLines(importLines())
    assertEquals(visibleTrees(map), 1812)

  test("calculates scenic score"):
    val map: Vector[Vector[Int]] =
      handleLines(Seq(
        "30373",
        "25512",
        "65332",
        "33549",
        "35390"
      ).map(_.map(_.asDigit)))
    assertEquals(calculateScenicScore(2, 1, map), 4)
    assertEquals(calculateScenicScore(2, 3, map), 8)

  test("calculates max scenic score for the example"):
    val map: Vector[Vector[Int]] =
      handleLines(Seq(
        "30373",
        "25512",
        "65332",
        "33549",
        "35390"
      ).map(_.map(_.asDigit)))
    assertEquals(maxScenicScore(map), 4)

  test("calculates max scenic score for the input"):
    val map = handleLines(importLines())
    assertEquals(maxScenicScore(map), 315495)
end Day08Suite
