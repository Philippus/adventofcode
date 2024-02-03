package adventofcode2021

import adventofcode2021.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("finds position of submarine for the example"):
    val course = Seq(
      "forward 5",
      "down 5",
      "forward 8",
      "up 3",
      "down 8",
      "forward 2"
    )
    assertEquals(findPositionOfSubmarine(course), 150)

  test("finds position of submarine for the input"):
    val course = importLines()
    assertEquals(findPositionOfSubmarine(course), 1893605)

  test("finds position of submarine with aim for the example"):
    val course = Seq(
      "forward 5",
      "down 5",
      "forward 8",
      "up 3",
      "down 8",
      "forward 2"
    )
    assertEquals(findPositionOfSubmarineWithAim(course), 900)

  test("finds position of submarine with aim for the input"):
    val course = importLines()
    assertEquals(findPositionOfSubmarineWithAim(course), 2120734350)
end Day02Suite
