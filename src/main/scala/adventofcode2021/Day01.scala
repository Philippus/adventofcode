package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day01:
  def measurementsLargerThanPrevious(s: Seq[Int]): Int =
    s.sliding(2).count(p => p.head < p.last)

  def measurementsLargerThanPreviousSums(s: Seq[Int]): Int =
    s.sliding(3).sliding(2).count(p => p.head.sum < p.last.sum)

  def importLines(): Seq[Int] =
    Using.resource(Source.fromResource("2021/day01input.txt")): source =>
      source.getLines().toSeq.map(_.toInt)
end Day01
