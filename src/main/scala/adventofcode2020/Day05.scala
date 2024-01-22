package adventofcode2020

import scala.io.Source
import scala.util.Using

object Day05:
  def determineRow(s: String): Int =
    val r  = 0.to(127)
    val f0 =
      r.filter(i => if s(0) == 'F' then r.indexOf(i) < (r.length + 1) / 2 else r.indexOf(i) >= (r.length + 1) / 2)
    val f1 =
      f0.filter(i => if s(1) == 'F' then f0.indexOf(i) < (f0.length + 1) / 2 else f0.indexOf(i) >= (f0.length + 1) / 2)
    val f2 =
      f1.filter(i => if s(2) == 'F' then f1.indexOf(i) < (f1.length + 1) / 2 else f1.indexOf(i) >= (f1.length + 1) / 2)
    val f3 =
      f2.filter(i => if s(3) == 'F' then f2.indexOf(i) < (f2.length + 1) / 2 else f2.indexOf(i) >= (f2.length + 1) / 2)
    val f4 =
      f3.filter(i => if s(4) == 'F' then f3.indexOf(i) < (f3.length + 1) / 2 else f3.indexOf(i) >= (f3.length + 1) / 2)
    val f5 =
      f4.filter(i => if s(5) == 'F' then f4.indexOf(i) < (f4.length + 1) / 2 else f4.indexOf(i) >= (f4.length + 1) / 2)
    val f6 =
      f5.filter(i => if s(6) == 'F' then f5.indexOf(i) < (f5.length + 1) / 2 else f5.indexOf(i) >= (f5.length + 1) / 2)
    f6.min

  def determineColumn(s: String): Int =
    val c  = 0.to(7)
    val c0 =
      c.filter(i => if s(7) == 'L' then c.indexOf(i) < (c.length + 1) / 2 else c.indexOf(i) >= (c.length + 1) / 2)
    val c1 =
      c0.filter(i => if s(8) == 'L' then c0.indexOf(i) < (c0.length + 1) / 2 else c0.indexOf(i) >= (c0.length + 1) / 2)
    val c2 =
      c1.filter(i => if s(9) == 'L' then c1.indexOf(i) < (c1.length + 1) / 2 else c1.indexOf(i) >= (c1.length + 1) / 2)
    c2.min

  def calculateSeatId(s: String): Int =
    determineRow(s) * 8 + determineColumn(s)

  def findMySeatId(seatIds: Seq[Int]): Int =
    seatIds.sorted.sliding(2).filter(seatIds => seatIds.last - seatIds.head == 2).min.head + 1

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2020/day05input.txt")): source =>
      source.getLines().toSeq
end Day05
