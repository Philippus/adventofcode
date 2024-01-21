package adventofcode2016

import adventofcode2016.Day16.*
import munit.FunSuite

class Day16Suite extends FunSuite:
  test("generates dragon curves"):
    assertEquals(dragonCurve("1", 3), "100")
    assertEquals(dragonCurve("0", 3), "001")
    assertEquals(dragonCurve("11111", 6), "11111000000")
    assertEquals(dragonCurve("111100001010", 15), "1111000010100101011110000")

  test("generates checksums"):
    assertEquals(generateChecksum("110010110100"), "100")

  test("fills disk"):
    assertEquals(fillDisk("10000", 20), "01100")

  test("fills disk for input"):
    assertEquals(fillDisk("11011110011011101", 272), "00000100100001100")

  test("fills disk for input part two"):
    assertEquals(fillDisk("11011110011011101", 35651584), "11011110011011101")
end Day16Suite
