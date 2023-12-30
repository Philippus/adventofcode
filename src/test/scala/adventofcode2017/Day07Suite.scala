package adventofcode2017

import adventofcode2017.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("determines bottom program"):
    val programs = readInputFile()
    assertEquals(determineBottomProgram(programs).map(_.name), Some("dgoocsw"))

  test("determines weight of unbalanced program"):
    val programs       = readInputFile()
    val bottomProgram  = determineBottomProgram(programs).get
    val expectedWeight = {
      val programsAbove            = bottomProgram.programsAbove
      val programsAboveWithWeights =
        bottomProgram.programsAbove.map(name => weightOfProgram(name, programs)).zip(programsAbove)
      val groupsWithSizes          = programsAboveWithWeights.groupBy(_._1).map(group => (group, group._2.length))
      groupsWithSizes.toSeq.minBy(-_._2)._1._1
    }
    assertEquals(findExpectedWeightOfUnbalancedProgram(bottomProgram.name, programs, expectedWeight), 1275)
end Day07Suite
