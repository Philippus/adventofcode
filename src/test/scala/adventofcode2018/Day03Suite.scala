package adventofcode2018

import adventofcode2018.Day03.*
import munit.FunSuite

class Day03Suite extends FunSuite:
  test("returns inches"):
    assertEquals(inches(Claim(123, 3, 2, 5, 4)).length, 20)

  test("returns overlaps"):
    val claims = Seq(
      Claim(1, 1, 3, 4, 4),
      Claim(2, 3, 1, 4, 4),
      Claim(3, 5, 5, 2, 2)
    )
    assertEquals(overlaps(claims).length, 4)

  test("returns overlaps for the input"):
    val claims = readInputFile().map(handleLine)
    assertEquals(overlaps(claims).length, 4)

  test("finds claim that doesn't overlap"):
    val claims = Seq(
      Claim(1, 1, 3, 4, 4),
      Claim(2, 3, 1, 4, 4),
      Claim(3, 5, 5, 2, 2)
    )
    assertEquals(doesntOverlap(claims).head.id, 3)

  test("finds claim that doesn't overlap for the input"):
    val claims = readInputFile().map(handleLine)
    assertEquals(doesntOverlap(claims).head.id, 1166)
end Day03Suite
