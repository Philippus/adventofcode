package adventofcode2017

import adventofcode2017.Day01.*
import munit.FunSuite

class Day01Suite extends FunSuite:
  test("sum matching digits"):
    assertEquals(sumFor("1122"), 3)
    assertEquals(sumFor("1111"), 4)
    assertEquals(sumFor("1234"), 0)
    assertEquals(sumFor("91212129"), 9)
    assertEquals(sumFor(readInputFile), 1150)

  test("sum matching halfway around digits"):
    assertEquals(sumHalfwayAround("1212"), 6)
    assertEquals(sumHalfwayAround("1221"), 0)
    assertEquals(sumHalfwayAround("123425"), 4)
    assertEquals(sumHalfwayAround("123123"), 12)
    assertEquals(sumHalfwayAround("12131415"), 4)
    assertEquals(sumHalfwayAround(readInputFile), 1064)
end Day01Suite
