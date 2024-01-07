package adventofcode2017

import adventofcode2017.Day18.*
import munit.FunSuite

class Day18Suite extends FunSuite:
  test("follows instructions of the example"):
    val instructions = Seq(
      "set a 1",
      "add a 2",
      "mul a a",
      "mod a 5",
      "snd a",
      "set a 0",
      "rcv a",
      "jgz a -1",
      "set a 1",
      "jgz a -2"
    )
    assertEquals(followInstructions(instructions), 4L)

  test("follows instructions of the input"):
    assertEquals(followInstructions(readInputFile()), 7071L)
end Day18Suite
