package adventofcode2023

import adventofcode2023.Day15.*
import munit.FunSuite

class Day15Suite extends FunSuite:
  test("calculates hash for HASH"):
    assertEquals(calculateHash("HASH"), 52L)

  test("calculates sum of hashes for the sample"):
    assertEquals(sumOfHashes("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"), 1320L)

  test("calculates sum of hashes for the input"):
    assertEquals(sumOfHashes(importLines()), 504449L)
end Day15Suite
