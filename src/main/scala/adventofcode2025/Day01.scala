package adventofcode2025

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day01:
  def dialAtZero(rotations: Seq[Int]): Int =
    rotations.scanLeft(50)(_ + _).count(_ % 100 == 0)

  def clicks(rotations: Seq[Int]): Int =
    @tailrec
    def loop(rotations: Seq[Int], curr: Int, acc: Int): Int =
      rotations match
        case Nil               =>
          acc
        case r :: rs if r == 0 =>
          loop(rs, curr, acc)
        case r :: rs           =>
          loop(r + r.sign * -1 :: rs, curr + r.sign, if (curr + r.sign) % 100 == 0 then acc + 1 else acc)
    loop(rotations, 50, 0)

  def importLines(): Seq[Int] =
    Using.resource(Source.fromResource("2025/day01input.txt")): source =>
      source.getLines().toSeq.map:
        case s"L$n" => n.toInt * -1
        case s"R$n" => n.toInt
end Day01
