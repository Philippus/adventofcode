package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day25:
  def overlaps(lockHeights: Seq[Int], keyHeights: Seq[Int]): Boolean =
    lockHeights.zip(keyHeights).exists:
      case (lh, kh) => lh + kh >= 6

  def heights(lockOrKey: Seq[String]): Seq[Int] =
    lockOrKey.transpose.map(_.count(_.==('#')) - 1)

  def sumNonOverlappingLockKeyPairs(locks: Seq[Seq[String]], keys: Seq[Seq[String]]): Int =
    (for
      lockHeights <- locks.map(heights)
      keyHeights  <- keys.map(heights)
      if !overlaps(lockHeights, keyHeights)
    yield 1).sum

  def handleLines(seq: Seq[String]): (Seq[Seq[String]], Seq[Seq[String]]) =
    seq.grouped(8).toSeq.map(_.take(7)).partition(_.head.startsWith("#####"))

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day25
