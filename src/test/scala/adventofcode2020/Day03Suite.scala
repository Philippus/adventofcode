package adventofcode2020

import adventofcode2020.Day03.*
import munit.FunSuite

class Day03Suite extends FunSuite:
  test("counts encountered trees"):
    val map = Seq(
      "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
    )
    val v   = handleLines(map)
    assertEquals(countTrees(3, 1, v), 7)

  test("counts encountered trees for the input"):
    val v = handleLines(readInputFile())
    assertEquals(countTrees(3, 1, v), 223)

  test("counts encountered trees for mutiple slopes"):
    val map = handleLines(Seq(
      "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
    ))
    assertEquals(checkSlopes(Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)), map), 336L)

  test("counts encountered trees for multiple slopes for the input"):
    val map = handleLines(readInputFile())
    assertEquals(checkSlopes(Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)), map), 3517401300L)
end Day03Suite
