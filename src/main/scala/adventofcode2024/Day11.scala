package adventofcode2024

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day11:
  val map: mutable.Map[(Long, Int), Long]  = mutable.Map[(Long, Int), Long]()
  def blink(stone: Long, times: Int): Long =
    if times == 0 then
      1L
    else
      map.getOrElseUpdate(
        (stone, times),
        stone match {
          case 0L =>
            blink(1L, times - 1)
          case s if s.toString.length % 2 == 0 =>
            blink(s.toString.take(s.toString.length / 2).toLong, times - 1) +
              blink(s.toString.drop(s.toString.length / 2).toLong, times - 1)
          case s =>
            blink(s * 2024L, times - 1)
        }
      )

  def countStones(stones: Seq[Long], times: Int): Long =
    stones.map(stone => blink(stone, times)).sum

  def importLines(): Seq[Long] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().take(1).flatMap(_.split(' ').map(_.toLong)).toSeq
end Day11
