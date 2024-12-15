package adventofcode2016

import scala.io.Source
import scala.util.Using

import nl.gn0s1s.between._

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

  def countValidIps(lines: Seq[String]): Long =
    val intervals: Seq[Interval[Long]] = lines.map: line =>
      line.split('-') match
        case Array(lo, hi) => Interval[Long](lo.toLong, hi.toLong + 1L).get
    .sortBy(_.`-`)
    val mergeIntervals                 = intervals.foldLeft(Seq.empty[Interval[Long]]) {
      case (Seq(), j) => Seq(j)
      case (s, j)     => s.last.findRelation(j) match
          case `<` | `>` => s :+ j
          case r         => s.init :+ s.last.span(j)
    }
    mergeIntervals.sliding(2).map(is => is.last.`-` - is.head.`+`).sum

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2016/day20input.txt")): source =>
      source.getLines().toSeq
end Day20
