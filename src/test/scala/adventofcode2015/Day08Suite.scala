package adventofcode2015

import adventofcode2015.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("calculates number of characters in memory"):
    assertEquals(numCharactersInMemory(""""""""), 0)
    assertEquals(numCharactersInMemory(""""abc""""), expected = 3)
    assertEquals(numCharactersInMemory(""""aaa\"aaa""""), expected = 7)
    assertEquals(numCharactersInMemory(""""\x27""""), expected = 1)

  test("calculates number of characters in memory for an encoded string"):
    assertEquals(numCharactersAfterEncoding(""""""""), 6)
    assertEquals(numCharactersAfterEncoding(""""abc""""), expected = 9)
    assertEquals(numCharactersAfterEncoding(""""aaa\"aaa""""), expected = 16)
    assertEquals(numCharactersAfterEncoding(""""\x27""""), expected = 11)

  test("calculates value for the input file"):
    assertEquals(calculateValueForFile, 1333)

  test("calculates value for the input file part two"):
    assertEquals(calculateValueForFilePartTwo, 2046)
end Day08Suite
