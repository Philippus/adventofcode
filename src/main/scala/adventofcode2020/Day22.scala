package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day22:
  @tailrec
  def play(player1: List[Int], player2: List[Int]): Long =
    if player1.isEmpty then
      player2.reverse.zipWithIndex.map((c, i) => c.toLong * (i + 1).toLong).sum
    else if player2.isEmpty then
      player1.reverse.zipWithIndex.map((c, i) => c.toLong * (i + 1).toLong).sum
    else
      (player1, player2) match {
        case (t1 :: r1, t2 :: r2) if t1 > t2 => play(r1 ++ List(t1, t2), r2)
        case (t1 :: r1, t2 :: r2) if t2 > t1 => play(r1, r2 ++ List(t2, t1))
      }

  def parse(input: String): (List[Int], List[Int]) =
    val split = input.split("\n\n")
    val deck1 = split.head.split("\n").tail.map(_.toInt).toList
    val deck2 = split.last.split("\n").tail.map(_.toInt).toList
    (deck1, deck2)

  def importLines(): String =
    Using.resource(Source.fromResource("2020/day22input.txt")): source =>
      source.mkString
end Day22
