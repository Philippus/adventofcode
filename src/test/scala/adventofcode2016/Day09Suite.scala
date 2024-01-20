package adventofcode2016

import adventofcode2016.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  test("decompresses sequence"):
    assertEquals(decompress("ADVENT"), 6)
    assertEquals(decompress("(3x3)XYZ"), 9)
    assertEquals(decompress("A(2x2)BCD(2x2)EFG"), 11)
    assertEquals(decompress("(6x1)(1x3)A"), 6)
    assertEquals(decompress("X(8x2)(3x3)ABCY"), 18)

  test("decompresses sequence for input file"):
    assertEquals(decompress(readInputfile().head), 99145)

  test("decompresses sequence for part two"):
    assertEquals(decompressPartTwo("(3x3)XYZ"), "XYZXYZXYZ".length.toLong)
    assertEquals(decompressPartTwo("X(8x2)(3x3)ABCY"), "XABCABCABCABCABCABCY".length.toLong)
    assertEquals(decompressPartTwo("(27x12)(20x12)(13x14)(7x10)(1x12)A"), 241920L)

  test("decompresses sequence for input file part two"):
    assertEquals(decompressPartTwo(readInputfile().head), 10943094568L)
end Day09Suite
