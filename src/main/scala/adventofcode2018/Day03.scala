package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03:
  case class Claim(id: Int, gapLeft: Int, gapTop: Int, width: Int, height: Int)

  def inches(c: Claim): Seq[(Int, Int)] =
    for
      w <- 1.to(c.width)
      h <- 1.to(c.height)
    yield (c.gapLeft + w, c.gapTop + h)

  def overlaps(c1: Claim, c2: Claim): Seq[(Int, Int)] =
    inches(c1).intersect(inches(c2))

  def overlaps(claims: Seq[Claim]): Seq[(Int, Int)] =
    claims.combinations(2).flatMap: combo =>
      overlaps(combo.head, combo.last)
    .distinct.toSeq

  def doesntOverlap(claims: Seq[Claim]): Seq[Claim] =
    val claimsWithOverlaps = claims.combinations(2).flatMap: combo =>
      if overlaps(combo.head, combo.last).nonEmpty then
        Seq(combo.head, combo.last)
      else
        Seq.empty
    claims.diff(claimsWithOverlaps.toSeq)

  def handleLine(s: String): Claim =
    s match
      case s"#$id @ $gapLeft,$gapRight: ${width}x$height" =>
        Claim(id.toInt, gapLeft.toInt, gapRight.toInt, width.toInt, height.toInt)

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2018/day03input.txt")):
      _.getLines().toSeq
end Day03
