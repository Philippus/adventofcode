package adventofcode2023

import adventofcode2023.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("calculates continuous groups"):
    assertEquals(calculateContinuousGroups("#.#.###"), "1,1,3")
    assertEquals(calculateContinuousGroups(".#.###.#.######"), "1,3,1,6")

  test("generates candidates"):
    assertEquals(generateCandidates("?"), Seq("#", "."))

  test("calculate arrangements"):
    assertEquals(calculatesArrangements(Seq(".??..??...?##. 1,1,3")), 4L)

  test("calculates arrangements for the input"):
    assertEquals(calculatesArrangements(importLines()), 7361L)

  test("unfolds records"):
    assertEquals(unfold(".# 1"), ".#?.#?.#?.#?.# 1,1,1,1,1")
    assertEquals(unfold("???.### 1,1,3"), "???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3")

//  test("calculates unfolded arrangements for the input"):
//    assertEquals(calculatesArrangements(importLines().map(unfold)), 7361L)

end Day12Suite
