package adventofcode2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03:
  case class Pos(x: Int, y: Int)

  def manhattanDistance(pos: Pos): Int = math.abs(pos.x) + math.abs(pos.y)

  def manhattanDistanceOfClosestIntersection(wires: (Seq[(Char, Int)], Seq[(Char, Int)])): Int =
    @tailrec
    def loop(wire: Seq[(Char, Int)], pos: Pos, acc: Seq[Pos]): Seq[Pos] =
      if wire.isEmpty then
        acc
      else
        val poss = wire.head match
          case ('R', amount) =>
            for
              i <- 1 to amount
            yield pos.copy(x = pos.x + i)
          case ('L', amount) =>
            for
              i <- 1 to amount
            yield pos.copy(x = pos.x - i)
          case ('U', amount) =>
            for
              i <- 1 to amount
            yield pos.copy(y = pos.y + i)
          case ('D', amount) =>
            for
              i <- 1 to amount
            yield pos.copy(y = pos.y - i)
        loop(wire.tail, poss.last, acc ++ poss)
    val poss1                                                           = loop(wires._1, Pos(0, 0), Seq.empty[Pos])
    val poss2                                                           = loop(wires._2, Pos(0, 0), Seq.empty[Pos])
    poss1.intersect(poss2).map(manhattanDistance).min

  def handleLines(lines: Seq[String]): Seq[Seq[(Char, Int)]] =
    lines.map: line =>
      line.split(',').map:
        case s"R$x" => ('R', x.toInt)
        case s"L$x" => ('L', x.toInt)
        case s"U$x" => ('U', x.toInt)
        case s"D$x" => ('D', x.toInt)
      .toSeq

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2019/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day03
