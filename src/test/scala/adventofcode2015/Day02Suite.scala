package adventofcode2015

import adventofcode2015.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("calculates the correct amount of wrapping paper needed for a box"):
    assertEquals(Box(2, 3, 4).wrappingPaperNeeded, 58)
    assertEquals(Box(1, 1, 10).wrappingPaperNeeded, 43)

  test("calculates the right amount of wrapping paper needed"):
    assertEquals(calculateWrappingPaperNeeded, 1598415)

  test("calculates the correct amount of ribbon needed for a box"):
    assertEquals(Box(2, 3, 4).ribbonNeeded, 34)
    assertEquals(Box(1, 1, 10).ribbonNeeded, 14)

  test("calculates the right amount of ribbon needed"):
    assertEquals(calculateRibbonNeeded, 3812909)

end Day02Suite
