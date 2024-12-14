package adventofcode2016

import adventofcode2016.Day23.*
import munit.FunSuite

class Day23Suite extends FunSuite:
  test("executes assembunnycode for the example"):
    val instructions = Seq(
      "cpy 2 a",
      "tgl a",
      "tgl a",
      "tgl a",
      "cpy 1 a",
      "dec a",
      "dec a"
    )
    val registers    = executeCode(instructions)
    assertEquals(registers('a'), 3L)

  test("executes assembunnycode for the input file"):
    val instructions = readInputfile()
    val registers    = executeCode(instructions, Map('a' -> 7L))
    assertEquals(registers('a'), 10584L)

  // replace
  // cpy b c
  // inc a
  // dec c
  // jnz c -2
  // dec d
  // jnz d -5
  // with
  // mul a b d
  // cpy 0 d
  // and change 'cpy -16 c' to 'cpy -12 c'
  test("executes assembunnycode for the input file to find the actual value"):
    val instructions = readInputfile()
    val registers    = executeCode(instructions, Map('a' -> 12L))
    assertEquals(registers('a'), 479007144L)
end Day23Suite
