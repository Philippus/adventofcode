package adventofcode2024

import scala.io.Source
import scala.util.Using

object Day02:
  private def isReportSafe(report: List[Int]): Boolean =
    List(report, report.reverse).exists: seq =>
      seq.sliding(2).forall:
        case List(a, b) => a < b && math.abs(a - b) <= 3
        case _          => false

  private def listsWithMaxOneRemoved(l: List[Int]): IndexedSeq[List[Int]] =
    def length = l.size
    for i <- 0 to length
    yield l.slice(0, i) ++ l.slice(i + 1, length)

  def countSafeReports(lines: Seq[String]): Int =
    lines.map(_.split("\\s+")).map: line =>
      isReportSafe(line.toList.map(_.toInt))
    .count(identity)

  def countSafeReportsWithProblemDampener(lines: Seq[String]): Int =
    lines.map(_.split("\\s+")).map: line =>
      listsWithMaxOneRemoved(line.toList.map(_.toInt)).exists(isReportSafe)
    .count(identity)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2024/day02input.txt")): source =>
      source.getLines().toSeq
end Day02
