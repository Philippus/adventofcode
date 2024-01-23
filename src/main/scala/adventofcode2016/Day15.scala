package adventofcode2016

import scala.annotation.tailrec

object Day15:
  case class Disc(positions: Int, startPosition: Int)

  def findTimeToPressButton(discs: Seq[Disc]): Int =
    @tailrec
    def loop(discsLeft: Seq[Disc], time: Int, startTime: Int): Int =
      if discsLeft.isEmpty then
        startTime
      else
        val disc = discsLeft.head
        if (startTime + time + disc.startPosition) % disc.positions == 0 then
          loop(discsLeft.tail, time + 1, startTime)
        else
          loop(discs, 1, startTime + 1)
    loop(discs, 1, 1)
end Day15
