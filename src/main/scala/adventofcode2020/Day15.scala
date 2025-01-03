package adventofcode2020

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day15:
  def nthSpokenNumber(numbers: List[Long], nth: Int): Long =
    val map: mutable.Map[Long, Int]        = mutable.Map[Long, Int]()
    @tailrec
    def loop(last: Long, round: Int): Long =
      if round == nth then
        last
      else
        if !map.contains(last) then
          map.update(last, round)
          loop(0L, round + 1)
        else
          val lastSpokenIdx       = round
          val nextToLastSpokenIdx = map(last)
          map.update(last, round)
          loop(lastSpokenIdx - nextToLastSpokenIdx, round + 1)

    numbers.init.zipWithIndex.foreach(i => map.update(i._1, i._2 + 1))
    loop(numbers.last, numbers.length)

  def importLines(): List[Long] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next().split(',').map(_.toLong).toList
end Day15
