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
end Day23Suite
