package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day13:
  // https://rosettacode.org/wiki/Chinese_remainder_theorem
  def chineseRemainder(n: List[Long], a: List[Long]): Long =
    val prod = n.product

    @tailrec
    def iter(n: List[Long], a: List[Long], sm: Long): Long =
      def mulInv(a: Long, b: Long): Long =
        @tailrec
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long =
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1

        if b == 1 then
          1
        else
          val x1 = loop(a, b, 0, 1)
          if x1 < 0 then x1 + b else x1

      if n.nonEmpty then
        val p = prod / n.head
        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      else
        sm

    iter(n, a, 0) % prod

  def determineEarliestBus(timestamp: Long, buses: Seq[Int]): Long =
    val (bus, wait) = buses.map: bus =>
      (bus, bus - (timestamp % bus))
    .minBy(_._2)
    bus * wait

  def determineEarliestTimestamp(buses: Seq[(Long, Long)]): Long =
    val modulos    = buses.map(_._1)
    val remainders = buses.map(bus => bus._1 - bus._2)
    chineseRemainder(modulos.toList, remainders.toList)

  def importLinesPartTwo(): Seq[(Long, Long)] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        val list = source.getLines().toList
        list.last.split(',').zipWithIndex.filterNot(_._1 == ("x")).map(x => (x._1.toLong, x._2.toLong))

  def importLines(): (Long, Seq[Int]) =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        val list = source.getLines().toList
        (list.head.toLong, list.last.split(',').filterNot(_.==("x")).map(_.toInt))
end Day13
