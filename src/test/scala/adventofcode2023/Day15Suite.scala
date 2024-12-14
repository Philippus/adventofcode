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

  test("fills boxes for the sample"):
    val boxes = fillBoxes("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
    assertEquals(boxes.head, Box(Seq(Lens("rn", 1L), Lens("cm", 2L))))
    assertEquals(boxes(3), Box(Seq(Lens("ot", 7L), Lens("ab", 5L), Lens("pc", 6L))))

  test("calculates focusing power for the sample"):
    val boxes = fillBoxes("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
    assertEquals(calculateFocusingPower(boxes), 145L)

  test("calculates focusing power for the input"):
    assertEquals(calculateFocusingPower(fillBoxes(importLines())), 145L)
end Day15Suite
