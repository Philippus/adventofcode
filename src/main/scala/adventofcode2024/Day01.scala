package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day01:
  
  def importLines(): Seq[Int] =
    Using.resource(Source.fromResource("2024/day01input.txt")): source =>
      source.getLines().toSeq.map(_.toInt)
end Day01
