package adventofcode2016

import adventofcode2016.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("initializes bots"):
    val instructions = Seq(
      "value 5 goes to bot 2",
      "bot 2 gives low to bot 1 and high to bot 0",
      "value 3 goes to bot 1",
      "bot 1 gives low to output 1 and high to bot 0",
      "bot 0 gives low to output 2 and high to output 0",
      "value 2 goes to bot 2"
    )
    assertEquals(initializeBots(instructions), Map(1 -> Set(3), 2 -> Set(2, 5)))

  test("follows instructions"):
    val instructions = Seq(
      "value 5 goes to bot 2",
      "bot 2 gives low to bot 1 and high to bot 0",
      "value 3 goes to bot 1",
      "bot 1 gives low to output 1 and high to bot 0",
      "bot 0 gives low to output 2 and high to output 0",
      "value 2 goes to bot 2"
    )
    assertEquals(findBot(instructions, initializeBots(instructions), Set(2, 5)), 2)

  test("finds bot for input file"):
    val instructions = readInputfile()
    assertEquals(findBot(instructions, initializeBots(instructions), Set(17, 61)), 161)

  test("gets output for input file"):
    val instructions = readInputfile()
    val outputs      = getOutputs(instructions, initializeBots(instructions))
    assertEquals(outputs(0) * outputs(1) * outputs(2), 133163)
end Day10Suite
