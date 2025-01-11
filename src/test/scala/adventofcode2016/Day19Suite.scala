package adventofcode2016

import scala.concurrent.duration.Duration

import adventofcode2016.Day19.*
import munit.FunSuite

class Day19Suite extends FunSuite:
  override val munitTimeout = Duration(1000, "s")
  test("finds Elf that gets all presents for the example"):
    assertEquals(getSafePosition(5), 3)

  test("finds Elf that gets all presents for the input"):
    assertEquals(getSafePosition(importLines()), 1834903)

  test("finds Elf that gets all presents directly across the circle for the sample"):
    assertEquals(stealPresentsDirectlyAcrossTheCircle(5), 2)

  test("finds Elf that gets all presents directly across the circle for the input"):
    assertEquals(stealPresentsDirectlyAcrossTheCircle(importLines()), 1420280)
end Day19Suite
