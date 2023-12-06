package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03:
  case class Pos(x: Int, y: Int)

  def determineVisitedHouses(line: String): Set[Pos] =
    @tailrec
    def helper(line: String, currentPosition: Pos, visitedHouses: Set[Pos]): Set[Pos] =
      if line.isEmpty then visitedHouses
      else
        val currentChar = line.head
        val nextPos     = currentChar match
          case '^' => currentPosition.copy(y = currentPosition.y + 1)
          case 'v' => currentPosition.copy(y = currentPosition.y - 1)
          case '>' => currentPosition.copy(x = currentPosition.x + 1)
          case '<' => currentPosition.copy(x = currentPosition.x - 1)
        helper(line.tail, nextPos, visitedHouses + nextPos)
    helper(line, Pos(0, 0), Set(Pos(0, 0)))

  def determineVisitedHousesWithRoboSanta(line: String): Set[Pos] =
    val odd  = line.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).mkString
    val even = line.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).mkString
    determineVisitedHouses(odd) ++ determineVisitedHouses(even)

  def calculatedVisitedHousesForFile: Int =
    Using.resource(Source.fromResource("2015/day03input.txt")):
      _.getLines().map(line =>
        determineVisitedHouses(line)
      ).toSeq.head.size

  def calculatedVisitedHousesWithRoboSantaForFile: Int =
    Using.resource(Source.fromResource("2015/day03input.txt")):
      _.getLines().map(line =>
        determineVisitedHousesWithRoboSanta(line)
      ).toSeq.head.size
end Day03
