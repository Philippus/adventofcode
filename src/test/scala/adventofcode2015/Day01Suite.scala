package adventofcode2015

import munit.FunSuite

import Day01.*

class Day01Suite extends FunSuite:
  test("finds the right floor"):
    assertEquals(findFloor("(())"), 0)
    assertEquals(findFloor("(()(()("), 3)
    assertEquals(findFloor("))("), -1)

  test("finds the right floor for the input file"):
    assertEquals(findFloorForInputFile, 74)

  test("finds the right position"):
    assertEquals(findFirstPositionInBasement("()())"), 5)

  test("finds the right position for the input file"):
    assertEquals(findPositionForInputFile, 1795)
end Day01Suite
