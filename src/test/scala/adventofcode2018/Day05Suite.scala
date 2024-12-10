package adventofcode2018

import scala.concurrent.duration.Duration

import adventofcode2018.Day05.*
import munit.FunSuite

class Day05Suite extends FunSuite:
  override val munitTimeout = Duration(500, "s")

  test("reacts polymer for the sample"):
    assertEquals(react("dabAcCaCBAcCcaDA"), 10)

  test("reacts polymer for the input"):
    assertEquals(react(importLines()), 10878)

  test("improves polymer for the sample"):
    assertEquals(improvePolymer("dabAcCaCBAcCcaDA"), 4)

  test("improves polymer for the input"):
    assertEquals(improvePolymer(importLines()), 6874)
end Day05Suite
