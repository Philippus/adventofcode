package adventofcode2017

import adventofcode2017.Day10.*
import adventofcode2017.Day14.*
import munit.FunSuite

class Day14Suite extends FunSuite:
  test("hashtobits"):
    assertEquals(hashToBits("a0c2017"), "1010000011000010000000010111")

  test("knot"):
    val a = stringToLengthsAndSuffix("flqrgnkx-0")
    assertEquals(hashToBits(Day10.processLengthsAsAscii(0.to(255), a)).take(8), "11010100")

  test("all knots"):
    val a = 0.to(127).map(i => stringToLengthsAndSuffix(s"jzgqcdpd-$i"))
    val b = a.map(aa => hashToBits(Day10.processLengthsAsAscii(0.to(255), aa)))
    assertEquals(b.map(c => c.count(d => d.==('1'))).sum, 3)
end Day14Suite
