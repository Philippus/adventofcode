package adventofcode2015

import adventofcode2015.Day25.*
import munit.FunSuite

class Day25Suite extends FunSuite:
  test("finds next code"):
    assertEquals(determineNextCode(startCode), BigInt(31916031L))
    assertEquals(determineNextCode(10071777L), BigInt(33071741))

  test("finds code by row and column"):
    assertEquals(findCodeByRowAndColumn(4, 2), BigInt(32451966L))
    assertEquals(findCodeByRowAndColumn(2947, 3029), BigInt(19980801L))
end Day25Suite
