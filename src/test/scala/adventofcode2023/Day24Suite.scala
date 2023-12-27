package adventofcode2023

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

import adventofcode2023.Day24.*
import munit.FunSuite

class Day24Suite extends FunSuite:
  test("should work for the example"):
    val h1    = Hailstone(19, 13, 30, -2, 1, -2)
    val h2    = Hailstone(18, 19, 22, -1, -1, -2)
    val h3    = Hailstone(20, 25, 34, -2, -2, -4)
    val h4    = Hailstone(12, 31, 28, -1, -2, -1)
    val h5    = Hailstone(20, 19, 15, 1, -5, -3)
    val l1    = hailstoneToLine(h1)
    val l2    = hailstoneToLine(h2)
    val l3    = hailstoneToLine(h3)
    val l4    = hailstoneToLine(h4)
    val l5    = hailstoneToLine(h5)
    val combs = Seq(l1, l2, l3, l4, l5).combinations(2).toSeq

    val intersections = combs.map(comb => intersection(comb.head, comb.last)).filter { i =>
      (if i._1 < i._3.hailstone.px then i._3.hailstone.vx < 0 else i._3.hailstone.vx > 0) &&
      (if i._2 < i._3.hailstone.py then i._3.hailstone.vy < 0 else i._3.hailstone.vy > 0) &&
      (if i._1 < i._4.hailstone.px then i._4.hailstone.vx < 0 else i._4.hailstone.vx > 0) &&
      (if i._2 < i._4.hailstone.py then i._4.hailstone.vy < 0 else i._4.hailstone.vy > 0)
    }

    assertEquals(intersections.count(i => i._1 >= 7L && i._1 <= 27L && i._2 >= 7L && i._2 <= 27L), 2)

  test("should work for input file"):
    val importedLines = importLines()
    val hailstones    = importedLines.map(handleLine)
    val lines         = hailstones.map(hailstoneToLine)
    val combs         = lines.combinations(2).toSeq.filterNot(comb => comb.head.slope == comb.last.slope)
    val intersections = combs.map(comb => intersection(comb.head, comb.last)).filter { i =>
      (if i._1 < i._3.hailstone.px then i._3.hailstone.vx < 0 else i._3.hailstone.vx > 0) &&
      (if i._2 < i._3.hailstone.py then i._3.hailstone.vy < 0 else i._3.hailstone.vy > 0) &&
      (if i._1 < i._4.hailstone.px then i._4.hailstone.vx < 0 else i._4.hailstone.vx > 0) &&
      (if i._2 < i._4.hailstone.py then i._4.hailstone.vy < 0 else i._4.hailstone.vy > 0)
    }

    assertEquals(
      intersections.count(i =>
        i._1 >= 200000000000000L && i._1 <= 400000000000000L && i._2 >= 200000000000000L && i._2 <= 400000000000000L
      ),
      31921
    )

end Day24Suite
