package adventofcode2020

import scala.io.Source
import scala.util.Using

object Day13:
  def determineEarliestBus(timestamp: Long, buses: Seq[Int]): Long =
    val (bus, wait) = buses.map: bus =>
      (bus, bus - (timestamp % bus))
    .minBy(_._2)
    bus * wait

  def importLines(): (Long, Seq[Int]) =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        val list = source.getLines().toList
        (list.head.toLong, list.last.split(',').filterNot(_.==("x")).map(_.toInt))
end Day13
