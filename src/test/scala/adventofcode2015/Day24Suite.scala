package adventofcode2015

import adventofcode2015.Day24.*
import munit.FunSuite

class Day24Suite extends FunSuite:
//  test("determines quantum entanglement of the first group of packages in the ideal configuration"):
//    val packages = Seq(1, 2, 3, 4, 5, 7, 8, 9, 10, 11)
//    assertEquals(determineGroups(packages), 99)
//    assertEquals(determineGroups(importLines()), 0)

  test("test"):
    val packages = importLines()
    assertEquals(determineMinimumCombinations(packages), BigInt(2))

//  test("weight of group"):
//    val packages = importLines()
//    assertEquals(determineWeightOfGroup(packages), BigInt(3))
end Day24Suite
