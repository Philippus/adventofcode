package adventofcode2017

import adventofcode2017.Day14.*
import munit.FunSuite

class Day14Suite extends FunSuite:
  test("converts a hash to bits"):
    assertEquals(hashToBits("a0c20170"), "10100000110000100000000101110000")

  test("creates knot hashes"):
    assertEquals(knotHash("flqrgnkx-0").take(8), "11010100")

  test("counts used squares for the sample"):
    assertEquals(countUsedSquares("flqrgnkx"), 8108)

  test("counts used squares for the input"):
    assertEquals(countUsedSquares(importLines()), 8074)

  test("draws the map"):
    println(drawGrid(createMapFromHash("flqrgnkx")))

  test("counts regions for the sample"):
    assertEquals(countRegions("flqrgnkx"), 1242)

  test("counts regions for the input"):
    assertEquals(countRegions(importLines()), 1212)
end Day14Suite
