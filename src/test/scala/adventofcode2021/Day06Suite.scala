package adventofcode2021

import adventofcode2021.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("determines next state"):
    val next =
      nextState("3,4,3,1,2".split(',').toSeq.map(_.toInt).groupBy(identity).map((k, v) => (k, v.length.toLong)))
    assertEquals(
      next.filterNot(_._2 == 0),
      "2,3,2,0,1".split(',').toSeq.map(_.toInt).groupBy(identity).map((k, v) => (k, v.length.toLong))
    )
    assertEquals(
      nextState(next).filterNot(_._2 == 0),
      "1,2,1,6,0,8".split(',').toSeq.map(_.toInt).groupBy(identity).map((k, v) => (k, v.length.toLong))
    )

  test("simulates lanternfish for the example"):
    assertEquals(
      simulateLanternfish(
        "3,4,3,1,2".split(',').toSeq.map(_.toInt).groupBy(identity).map((k, v) => (k, v.length.toLong)),
        18
      ),
      26L
    )
    assertEquals(
      simulateLanternfish(
        "3,4,3,1,2".split(',').toSeq.map(_.toInt).groupBy(identity).map((k, v) => (k, v.length.toLong)),
        80
      ),
      5934L
    )
    assertEquals(
      simulateLanternfish(
        "3,4,3,1,2".split(',').toSeq.map(_.toInt).groupBy(identity).map((k, v) => (k, v.length.toLong)),
        256
      ),
      26984457539L
    )

  test("simulates lanternfish for the input"):
    assertEquals(simulateLanternfish(importLines(), 80), 352872L)
    assertEquals(simulateLanternfish(importLines(), 256), 1604361182149L)
end Day06Suite
