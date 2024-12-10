package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day05:
  @tailrec
  def react(polymer: String): Int =
    polymer.sliding(2).find(x => math.abs(x.head - x.last) == 32) match {
      case Some(value) => react(polymer.replaceFirst(value, ""))
      case None        => polymer.length
    }

  def improvePolymer(polymer: String): Int =
    (for
      i <- 'A' to 'Z'
    yield react(polymer.replace(i.toString, "").replace((i.toInt + 32).toChar.toString, ""))).min

  def importLines(): String =
    Using.resource(Source.fromResource("2018/day05input.txt")):
      _.getLines().toSeq.head
end Day05
