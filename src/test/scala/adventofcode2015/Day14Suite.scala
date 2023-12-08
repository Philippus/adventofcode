package adventofcode2015

import adventofcode2015.Day14.*
import munit.FunSuite

class Day14Suite extends FunSuite:
  test("finds distance travelled by a reindeer"):
    assertEquals(traveled(Reindeer("Dancer", 16, 11, 162), 1), 16)
    assertEquals(traveled(Reindeer("Comet", 14, 10, 127), 3), 42)
    assertEquals(traveled(Reindeer("Comet", 14, 10, 127), 1000), 1120)
    assertEquals(traveled(Reindeer("Dancer", 16, 11, 162), 1000), 1056)

  test("calculates score for new scoring system"):
    assertEquals(
      raceReindeer(
        Seq(
          Reindeer("Comet", 14, 10, 127),
          Reindeer("Dancer", 16, 11, 162)
        ),
        1000
      ),
      689
    )

  test("finds distance travelled by winning reindeer"):
    assertEquals(distanceTraveledByWinningReindeer, 2660)

  test("calculates score of winning reindeer"):
    assertEquals(scoreOfWinningReindeer, 1256)
end Day14Suite
