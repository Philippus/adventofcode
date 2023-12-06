package adventofcode2015

import scala.io.Source
import scala.util.Using

object Day02:
  case class Box(l: Int, w: Int, h: Int):
    val wrappingPaperNeeded = 2 * l * w + 2 * w * h + 2 * h * l +
      Seq(l * w, w * h, h * l).min
    val sides               = Seq(l, w, h).sorted.init
    val ribbonNeeded        = sides.head + sides.head + sides.last + sides.last + l * w * h

  def calculateWrappingPaperNeeded: Int =
    Using.resource(Source.fromResource("2015/day02input.txt")):
      _.getLines().map(line =>
        val (l, w, h) = line match
          case s"${ls}x${ws}x${hs}" => (ls.toInt, ws.toInt, hs.toInt)
        Box(l, w, h).wrappingPaperNeeded
      ).sum

  def calculateRibbonNeeded: Int =
    Using.resource(Source.fromResource("2015/day02input.txt")):
      _.getLines().map(line =>
        val (l, w, h) = line match
          case s"${ls}x${ws}x${hs}" => (ls.toInt, ws.toInt, hs.toInt)
        Box(l, w, h).ribbonNeeded
      ).sum
end Day02
