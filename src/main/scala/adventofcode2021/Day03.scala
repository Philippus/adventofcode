package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03:
  def gammaRate(numbers: Seq[String]): String =
    (for
      i <- numbers.head.indices
    yield numbers.map(number => number(i)).groupBy(identity).toSeq.maxBy(_._2.length)._1).mkString

  def epsilonRate(numbers: Seq[String]): String =
    (for
      i <- numbers.head.indices
    yield numbers.map(number => number(i)).groupBy(identity).toSeq.minBy(_._2.length)._1).mkString

  def powerConsumption(gammaRate: String, epsilonRate: String): Int =
    Integer.parseInt(gammaRate, 2) * Integer.parseInt(epsilonRate, 2)

  def oxygenGeneratorRating(numbers: Seq[String]): String =
    @tailrec
    def loop(numbers: Seq[String], index: Int, acc: String): String =
      if index >= numbers.head.length then
        acc
      else
        val c = numbers.map(number => number(index)).groupBy(identity).toSeq.minBy(x => (-x._2.length, -x._1))._1
        loop(numbers.filter(number => number(index) == c), index + 1, acc :+ c)

    loop(numbers, 0, "")

  def co2ScrubberRating(numbers: Seq[String]): String =
    @tailrec
    def loop(numbers: Seq[String], index: Int, acc: String): String =
      if index >= numbers.head.length then
        acc
      else
        val c = numbers.map(number => number(index)).groupBy(identity).toSeq.minBy(x => (x._2.length, x._1))._1
        loop(numbers.filter(number => number(index) == c), index + 1, acc :+ c)

    loop(numbers, 0, "")

  def lifeSupportRating(oxygenGeneratorRating: String, co2ScrubberRating: String): Int =
    Integer.parseInt(oxygenGeneratorRating, 2) * Integer.parseInt(co2ScrubberRating, 2)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2021/day03input.txt")): source =>
      source.getLines().toSeq
end Day03
