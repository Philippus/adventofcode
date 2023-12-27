package adventofcode2023

import scala.io.Source
import scala.util.Using

object Day24:
  case class Hailstone(px: Double, py: Double, pz: Double, vx: Double, vy: Double, vz: Double)

  case class Line(slope: Double, intercept: Double, hailstone: Hailstone)

  def intersection(line1: Line, line2: Line): (Double, Double, Line, Line) =
    if line1.slope == line2.slope then (Double.MaxValue, Double.MaxValue, line1, line2)
    else {
      val x = (line2.intercept - line1.intercept) / (line1.slope - line2.slope)
      val y = line1.slope * x + line1.intercept

      (x, y, line1, line2)
    }

  def hailstoneToLine(hailstone: Hailstone): Line =
    val slope     = (hailstone.py - (hailstone.py + hailstone.vy)) / (hailstone.px - (hailstone.px + hailstone.vx))
    val intercept = hailstone.py - (hailstone.px * slope)
    Line(slope, intercept, hailstone)

  def handleLine(line: String): Hailstone = line match
    case s"$px, $py, $pz @ $vx, $vy, $vz" =>
      Hailstone(px.toDouble, py.toDouble, pz.toDouble, vx.toDouble, vy.toDouble, vz.toDouble)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2023/day24input.txt")):
      _.getLines().toSeq
end Day24
