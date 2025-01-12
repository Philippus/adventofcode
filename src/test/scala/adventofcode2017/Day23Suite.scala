package adventofcode2017

import scala.concurrent.duration.Duration

import adventofcode2017.Day23.*
import munit.FunSuite

class Day23Suite extends FunSuite:
  override val munitTimeout = Duration(500, "s")
  test("sums invocations of mul"):
    assertEquals(followInstructions(importLines()), 5929L)

  test("runs patched program"):
    assertEquals(followPatchedInstructions(importLines()), 907L)
end Day23Suite
