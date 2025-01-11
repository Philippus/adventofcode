package adventofcode2017

import adventofcode2017.Day11.*
import munit.FunSuite

class Day11Suite extends FunSuite:
  test("finds targe position for the samples"):
    assertEquals(findTargetPos("ne,ne,ne".split(',')), Pos(-3, 0))
    assertEquals(findTargetPos("ne,ne,sw,sw".split(',')), Pos(0, 0))
    assertEquals(findTargetPos("ne,ne,s,s".split(',')), Pos(-2, 2))
    assertEquals(findTargetPos("se,sw,se,sw,sw".split(',')), Pos(1, 2))

  test("finds fewest steps to child process for the samples"):
    assertEquals(stepsToTarget("ne,ne,ne".split(',')), 3)
    assertEquals(stepsToTarget("ne,ne,sw,sw".split(',')), 0)
    assertEquals(stepsToTarget("ne,ne,s,s".split(',')), 2)
    assertEquals(stepsToTarget("se,sw,se,sw,sw".split(',')), 3)

  test("finds fewest steps to child process for the input"):
    assertEquals(stepsToTarget(importLines()), 810)

  test("finds steps to furthest position child process got for the input"):
    assertEquals(stepsToFurthestPosition(importLines()), 1567)
end Day11Suite
