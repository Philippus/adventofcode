package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10:
  def countJoltageDifferences(adapters: List[Int]): Int =
    (0 +: adapters :+ adapters.max + 3).sorted.sliding(2).toSeq.map:
      case Seq(a, b) => b - a
    .partition(_.==(1)) match
      case (p1, p3) => p1.length * p3.length

  def countArrangements(adapters: List[Int]): Long =
    @tailrec
    def loop(adapterDiffs: List[Int], acc: Long): Long =
      if adapterDiffs.isEmpty then
        acc
      else
        adapterDiffs match
          case 3 :: _                     => loop(adapterDiffs.drop(1), acc)
          case 1 :: 3 :: _                => loop(adapterDiffs.drop(2), acc)
          case 1 :: 1 :: 3 :: _           => loop(adapterDiffs.drop(3), acc * 2)
          case 1 :: 1 :: 1 :: 3 :: _      => loop(adapterDiffs.drop(4), acc * 4)
          case 1 :: 1 :: 1 :: 1 :: 3 :: _ => loop(adapterDiffs.drop(5), acc * 7)

    val adapterDiffs = (0 +: adapters :+ adapters.max + 3).sorted.sliding(2).toList.map:
      case Seq(a, b) => b - a
    loop(adapterDiffs, 1L)

  def importLines(): List[Int] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList.map(_.toInt)
end Day10
