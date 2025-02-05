package adventofcode2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day09:
  def highestScore(players: Int, lastMarble: Int): Long =
    val marbles                                                                                         = mutable.ArrayBuffer(0)
    @tailrec
    def loop(nextPlayer: Int, currentMarble: Int, nextMarble: Int, acc: Map[Int, Long]): Map[Int, Long] =
      if nextMarble > lastMarble then
        acc
      else
        if nextMarble % 23 == 0 then
          val deletionPoint = (((currentMarble - 7) % marbles.length) + marbles.length) % marbles.length
          val removedMarble = marbles(deletionPoint)
          marbles.remove(deletionPoint)
          loop(
            if (nextPlayer + 1) > players then 1 else nextPlayer + 1,
            deletionPoint,
            nextMarble + 1,
            acc.updated(nextPlayer, acc.getOrElse(nextPlayer, 0L) + nextMarble + removedMarble)
          )
        else
          val insertionPoint = (currentMarble + 1) % marbles.length
          marbles.insert(insertionPoint + 1, nextMarble)
          loop(
            if (nextPlayer + 1) > players then 1 else nextPlayer + 1,
            (insertionPoint + 1) % marbles.length,
            nextMarble + 1,
            acc
          )

    loop(1, 0, 1, Map.empty[Int, Long]).maxBy(_._2)._2

  def importLines(): (Int, Int) =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next() match
          case s"$n players; last marble is worth $ps points" => (n.toInt, ps.toInt)
end Day09
