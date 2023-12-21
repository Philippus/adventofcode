package adventofcode2018

import adventofcode2018.Day01.*
import munit.FunSuite

class Day01Suite extends FunSuite:
  test("applies frequencies"):
    assertEquals(readInputFile().sum, 408)

  test("finds first frequency reached twice"):
    assertEquals(findFirstFrequencyTwice(Seq(+3, +3, +4, -2, -4)), 10)
    assertEquals(findFirstFrequencyTwice(readInputFile()), 55250)
end Day01Suite
