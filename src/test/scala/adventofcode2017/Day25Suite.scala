package adventofcode2017

import adventofcode2017.Day25.*
import munit.FunSuite

class Day25Suite extends FunSuite:
  test("follows blueprint for the example"):
    assertEquals(followBlueprint(), 3L)

  test("follows blueprint for the input file"):
    assertEquals(followBlueprintForInput(), 4287L)
end Day25Suite
