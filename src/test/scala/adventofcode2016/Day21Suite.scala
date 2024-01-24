package adventofcode2016

import adventofcode2016.Day21.*
import munit.FunSuite

class Day21Suite extends FunSuite:
  test("scrambles string"):
    assertEquals(scramble(Seq("swap position 4 with position 0"), "abcde"), "ebcda")
    assertEquals(scramble(Seq("swap letter d with letter b"), "ebcda"), "edcba")
    assertEquals(scramble(Seq("reverse positions 0 through 4"), "edcba"), "abcde")
    assertEquals(scramble(Seq("rotate left 1 step"), "abcde"), "bcdea")
    assertEquals(scramble(Seq("rotate right 2 steps"), "abcde"), "deabc")
    assertEquals(scramble(Seq("move position 1 to position 4"), "bcdea"), "bdeac")
    assertEquals(scramble(Seq("move position 3 to position 0"), "bdeac"), "abdec")
    assertEquals(scramble(Seq("rotate based on position of letter b"), "abdec"), "ecabd")
    assertEquals(scramble(Seq("rotate based on position of letter d"), "ecabd"), "decab")

  test("scrambles string for the example"):
    val instructions = Seq(
      "swap position 4 with position 0",
      "swap letter d with letter b",
      "reverse positions 0 through 4",
      "rotate left 1 step",
      "move position 1 to position 4",
      "move position 3 to position 0",
      "rotate based on position of letter b",
      "rotate based on position of letter d"
    )
    assertEquals(scramble(instructions, "abcde"), "decab")

  test("scrambles string for the input"):
    val instructions = readInputfile()
    assertEquals(scramble(instructions, "abcdefgh"), "bgfacdeh")

  test("finds password for the input"):
    val instructions = readInputfile()
    assertEquals(findPassword(instructions, "fbgdceah"), "bdgheacf")
end Day21Suite
