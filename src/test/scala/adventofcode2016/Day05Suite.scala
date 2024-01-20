package adventofcode2016

import adventofcode2016.Day05.*
import munit.FunSuite

class Day05Suite extends FunSuite:
  test("finds password for example"):
    assertEquals(determinePassword("abc"), "18f47a30")

  test("finds password for input"):
    assertEquals(determinePassword("ojvtpuvg"), "4543c154")

  test("finds password for example part two"):
    assertEquals(determinePasswordPartTwo("abc"), "05ace8e3")

  test("finds password for input part two"):
    assertEquals(determinePasswordPartTwo("ojvtpuvg"), "1050cbbd")
end Day05Suite
