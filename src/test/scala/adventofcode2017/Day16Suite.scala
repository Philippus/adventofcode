package adventofcode2017

import adventofcode2017.Day16.*
import munit.FunSuite

class Day16Suite extends FunSuite:
  test("spin"):
    assertEquals(spin("abcde", 1), "eabcd")

  test("exchange"):
    assertEquals(exchange("eabcd", 3, 4), "eabdc")

  test("partner"):
    assertEquals(partner("eabdc", 'e', 'b'), "baedc")

  test("dance"):
    assertEquals(dance(Seq("s1", "x3/4", "pe/b"), "abcde", 1), "baedc")

  test("dance with input file"):
    assertEquals(dance(readInputFile(), "abcdefghijklmnop", 1), "iabmedjhclofgknp")

  test("dance multiple times"):
    assertEquals(dance(Seq("s1", "x3/4", "pe/b"), "abcde", 2), "ceadb")

  test("dance with input file"):
    val cycleLength = findCycleLength(readInputFile(), "abcdefghijklmnop")
    assertEquals(cycleLength, 36L)
    assertEquals(dance(readInputFile(), "abcdefghijklmnop", 1_000L % cycleLength), "oildcmfeajhbpngk")
    assertEquals(dance(readInputFile(), "abcdefghijklmnop", 2_000L % cycleLength), "iabdhpmlgjeoknfc")
    assertEquals(dance(readInputFile(), "abcdefghijklmnop", 3_000L % cycleLength), "afodekphijbgcnml")
    assertEquals(dance(readInputFile(), "abcdefghijklmnop", 1_000_000_000 % cycleLength), "oildcmfeajhbpngk")
end Day16Suite
