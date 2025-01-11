package adventofcode2016

import adventofcode2016.Day22.*
import munit.FunSuite

class Day22Suite extends FunSuite:
  test("determines viable pairs"):
    assertEquals(viablePairs(importLines().map(parseLine)), 985)

  test("draw grid and count by hand"):
    println(drawGrid(importLines().map(parseLine)))

    /*
    3 steps left, to get around the "wall"
    20 steps up
    5 steps right
    moving the G one step left and moving the empty node back left to G takes 5 steps
    repeat this 30 times
    + 1 step to move G to the left
     */
end Day22Suite
