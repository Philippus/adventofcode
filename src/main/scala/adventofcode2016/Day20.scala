package adventofcode2016

import scala.io.Source
import scala.util.Using

object Day20:
  def lowestValueIp(lines: Seq[String]): Long =
    val intervals = lines.map: line =>
      line.split('-') match
        case Array(lo, hi) => (lo.toLong, hi.toLong)
    var found     = false
    var j         = 0L
    while !found && j <= 4294967295L do
      if intervals.forall(i => j < i._1 || j > i._2) then
        found = true
      else
        j += 1
    j

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2016/day20input.txt")): source =>
      source.getLines().toSeq
end Day20
