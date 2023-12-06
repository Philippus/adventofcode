package adventofcode2015

import adventofcode2015.Day11.*
import munit.FunSuite

class Day11Suite extends FunSuite:
  test("validates a password"):
    assertEquals(isValid("hijklmmn"), false)
    assertEquals(isValid("abbceffg"), false)
    assertEquals(isValid("abbcegjk"), false)
    assertEquals(isValid("abcdffaa"), true)
    assertEquals(isValid("ghjaabcc"), true)

  test("determines the next password"):
    assertEquals(nextPassword("abcdefgh"), "abcdffaa")
    assertEquals(nextPassword("ghijklmn"), "ghjaabcc")

  test("determines the next password for the puzzle input"):
    assertEquals(nextPassword("cqjxjnds"), "cqjxxyzz")

  test("determines the next password for the puzzle input part two"):
    assertEquals(nextPassword("cqjxxyzz"), "cqkaabcc")
end Day11Suite
