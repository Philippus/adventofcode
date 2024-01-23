package adventofcode2016

import adventofcode2016.Day15.*
import munit.FunSuite

class Day15Suite extends FunSuite:
  test("finds time to press button for the example"):
    assertEquals(findTimeToPressButton(Seq(Disc(5, 4), Disc(2, 1))), 5)

  test("finds time to press button for the input"):
    val discs = Seq(
      Disc(13, 11),
      Disc(5, 0),
      Disc(17, 11),
      Disc(3, 0),
      Disc(7, 2),
      Disc(19, 17)
    )
    assertEquals(findTimeToPressButton(discs), 122318)

  test("finds time to press button for the input part two"):
    val discs = Seq(
      Disc(13, 11),
      Disc(5, 0),
      Disc(17, 11),
      Disc(3, 0),
      Disc(7, 2),
      Disc(19, 17),
      Disc(11, 0)
    )
    assertEquals(findTimeToPressButton(discs), 122318)
end Day15Suite
