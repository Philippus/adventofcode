package adventofcode2018

import adventofcode2018.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("contains two"):
    assert(exactlyTwo("bababc"))

  test("contains three"):
    assert(exactlyTwo("bababc"))

  test("produces checksum"):
    val boxIds = Seq(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )
    assertEquals(checksum(boxIds), 12)

  test("produces checksum for input"):
    val boxIds = readInputFile()
    assertEquals(checksum(boxIds), 5952)

  test("common letters between box ids"):
    val boxIds = Seq(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"
    )
    assertEquals(commonBetweenBoxIds(boxIds), "fgij")

  test("common letters between box ids for input"):
    val boxIds = readInputFile()
    assertEquals(commonBetweenBoxIds(boxIds), "fgij")
end Day02Suite
