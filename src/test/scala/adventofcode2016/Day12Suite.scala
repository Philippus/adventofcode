package adventofcode2016

import adventofcode2016.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("executes assembunnycode for the example"):
    val instructions = Seq(
      "cpy 41 a",
      "inc a",
      "inc a",
      "dec a",
      "jnz a 2",
      "dec a"
    )
    val registers    = executeCode(instructions)
    assertEquals(registers('a'), 42L)

  test("executes assembunnycode for the input file"):
    val instructions = readInputfile()
    val registers    = executeCode(instructions)
    assertEquals(registers('a'), 318003L)

  test("executes assembunnycode for the input file part two"):
    val instructions = readInputfile()
    val registers    = executeCode(instructions, Map('c' -> 1L))
    assertEquals(registers('a'), 9227657L)
end Day12Suite
