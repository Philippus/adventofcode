package adventofcode2024

import scala.io.Source
import scala.util.Using

object Day01:
  private def createListsFromInput(lines: Seq[String]): (Seq[Int], Seq[Int]) =
    val seqTuples: Seq[(Int, Int)] = lines.map:
      case s"$a   $b" => (a.toInt, b.toInt)
    seqTuples.unzip

  def distanceBetweenLists(lines: Seq[String]): Int =
    val (left, right) = createListsFromInput(lines)
    left.sorted.zip(right.sorted).map((l, r) => scala.math.abs(r - l)).sum

  def similarityBetweenLists(lines: Seq[String]): Int =
    val (left, right) = createListsFromInput(lines)
    left.map(l => right.count(_ == l) * l).sum

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2024/day01input.txt")): source =>
      source.getLines().toSeq
end Day01
