package adventofcode2017

import adventofcode2017.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  test("scores groups for the examples"):
    assertEquals(handleString("{}"), 1)
    assertEquals(handleString("{{{}}}"), 6)
    assertEquals(handleString("{{},{}}"), 5)
    assertEquals(handleString("{{{},{},{{}}}}"), 16)
    assertEquals(handleString("{{{},{{}},{{}}}}"), 20)
    assertEquals(handleString("{<a>,<a>,<a>,<a>}"), 1)
    assertEquals(handleString("{<a>,<a>,<a>,{}}"), 3)
    assertEquals(handleString("{{<ab>},{<ab>},{<ab>},{<ab>}}"), 9)
    assertEquals(handleString("{{<!!>},{<!!>},{<!!>},{<!!>}}"), 9)
    assertEquals(handleString("{{<a!>},{<a!>},{<a!>},{<ab>}}"), 3)

  test("scores groups for the input file"):
    assertEquals(handleString(readInputFile()), 13154)

  test("counts garbage for the examples"):
    assertEquals(handleStringPt2("{}"), 0)
    assertEquals(handleStringPt2("{<>}"), 0)
    assertEquals(handleStringPt2("{<random characters>}"), 17)
    assertEquals(handleStringPt2("{<<<<>}"), 3)
    assertEquals(handleStringPt2("{<{!>}>}"), 2)
    assertEquals(handleStringPt2("{<!!>}"), 0)
    assertEquals(handleStringPt2("{<!!!>>}"), 0)
    assertEquals(handleStringPt2("{<{o\"i!a,<{i<a>}"), 10)

  test("counts garbage for the input file"):
    assertEquals(handleStringPt2(readInputFile()), 6369)
end Day09Suite
