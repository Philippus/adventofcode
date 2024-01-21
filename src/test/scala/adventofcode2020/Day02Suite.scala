package adventofcode2020

import adventofcode2020.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("validates password"):
    assert(validatePassword("1-3 a: abcde"))
    assert(!validatePassword("1-3 b: cdefg"))
    assert(validatePassword("2-9 c: ccccccccc"))

  test("validates passwords"):
    assertEquals(validatePasswords(Seq("1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc")), 2)

  test("validates passwords in input file"):
    val candidates = readInputFile()
    assertEquals(validatePasswords(candidates), 560)

  test("validates password part two"):
    assert(validatePasswordPartTwo("1-3 a: abcde"))
    assert(!validatePasswordPartTwo("1-3 b: cdefg"))
    assert(!validatePasswordPartTwo("2-9 c: ccccccccc"))

  test("validates passwords in input file part two"):
    val candidates = readInputFile()
    assertEquals(validatePasswordsPartTwo(candidates), 560)
end Day02Suite
