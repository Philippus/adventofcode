package adventofcode2016

import scala.concurrent.duration.Duration

import adventofcode2016.Day19.*
import munit.FunSuite

class Day19Suite extends FunSuite:
  override val munitTimeout = Duration(1000, "s")
  test("finds Elf that getsa all presents for the example"):
    assertEquals(stealPresents(5), 3)

  test("finds Elf that gets all presents for the input"):
    assertEquals(stealPresents(importLines()), 1834903)
end Day19Suite
