package adventofcode2020

import scala.io.Source
import scala.util.Using

object Day10:
  def countJoltageDifferences(adapters: List[Int]): Int =
    (0 +: adapters :+ adapters.max + 3).sorted.sliding(2).toSeq.map:
      case Seq(a, b) => b - a
    .partition(_.==(1)) match
      case (p1, p3) => p1.length * p3.length

  def importLines(): List[Int] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList.map(_.toInt)
end Day10
