package adventofcode2023

import adventofcode2023.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  test("calculates sequence of differences"):
    assertEquals(
      sequenceOfDifferences(Seq(0, 3, 6, 9, 12, 15)),
      Seq(Seq(0, 3, 6, 9, 12, 15), Seq.fill(5)(3), Seq.fill(4)(0))
    )

  test("predicts next value"):
    assertEquals(predictNextValue(sequenceOfDifferences(Seq(0, 3, 6, 9, 12, 15))), 18)
    assertEquals(predictNextValue(sequenceOfDifferences(Seq(10, 13, 16, 21, 30, 45))), 68)

  test("sums extrapolated next values for the input"):
    assertEquals(sumExtrapolatedNextValues(importLines()), 1877825184)

  test("predicts previous value"):
    assertEquals(predictPreviousValue(sequenceOfDifferences(Seq(10, 13, 16, 21, 30, 45))), 5)

  test("sums extrapolated previous values for the input"):
    assertEquals(sumExtrapolatedPreviousValues(importLines()), 1108)
end Day09Suite
