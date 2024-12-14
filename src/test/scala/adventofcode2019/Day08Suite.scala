package adventofcode2019

import adventofcode2019.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("parses into layers"):
    assertEquals(parse("123456789012", 3, 2), Seq("123456", "789012"))

  test("counts digits"):
    assertEquals(countDigit("789012", '0'), 1)

  test("calculates layer value"):
    assertEquals(calculateLayer(parse(importLines(), 25, 6)), 2125)

  test("find image for the example"):
    assertEquals(findPixels(parse("0222112222120000", 2, 2)), "0110")

  test("draws image for the input"):
    drawImage(findPixels(parse(importLines(), 25, 6))) // JYZHF
    assert(true)
end Day08Suite
