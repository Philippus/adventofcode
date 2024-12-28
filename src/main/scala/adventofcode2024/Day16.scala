package adventofcode2024

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

import adventofcode2024.Day16.Direction.*

object Day16:
  case class Point(x: Int, y: Int, char: Char)

  enum Direction:
    case East  extends Direction
    case West  extends Direction
    case North extends Direction
    case South extends Direction
  end Direction

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(map: Map[(Int, Int), Char], width: Int, height: Int): String =
    val grid = createGrid(width, height)
    createGrid(width, height).map:
      case (0, y) => "\n" + map(0, y)
      case (x, y) => map(x, y)
    .mkString

  def lowestScoreForReindeer(map: Map[(Int, Int), Char]): Long =
    val unvisitedNorth: mutable.Map[(Int, Int, Direction), Long] = map
      .filter(p => p._2 == 'S' || p._2 == '.' || p._2 == 'E')
      .map(p => (p._1._1, p._1._2, North) -> Long.MaxValue).to(mutable.Map)

    val unvisitedSouth: mutable.Map[(Int, Int, Direction), Long] = map
      .filter(p => p._2 == 'S' || p._2 == '.' || p._2 == 'E')
      .map(p => (p._1._1, p._1._2, South) -> Long.MaxValue).to(mutable.Map)

    val unvisitedEast: mutable.Map[(Int, Int, Direction), Long] = map
      .filter(p => p._2 == 'S' || p._2 == '.' || p._2 == 'E')
      .map(p => if p._2 == 'S' then (p._1._1, p._1._2, East) -> 0L else (p._1._1, p._1._2, East) -> Long.MaxValue).to(
        mutable.Map
      )

    val unvisitedWest: mutable.Map[(Int, Int, Direction), Long] = map
      .filter(p => p._2 == 'S' || p._2 == '.' || p._2 == 'E')
      .map(p => (p._1._1, p._1._2, West) -> Long.MaxValue).to(mutable.Map)

    val unvisited = unvisitedNorth ++ unvisitedSouth ++ unvisitedEast ++ unvisitedWest

    val visited = mutable.Map[(Int, Int, Direction), Long]()
    while unvisited.exists(p => p._2 < Long.MaxValue) do
      val currentNode = unvisited.minBy(_._2)
      // forward
      currentNode._1 match
        case (x, y, East) if unvisited.contains((x + 1, y, East))   =>
          unvisited.update((x + 1, y, East), math.min(currentNode._2 + 1, unvisited((x + 1, y, East))))
        case (x, y, West) if unvisited.contains((x - 1, y, West))   =>
          unvisited.update((x - 1, y, West), math.min(currentNode._2 + 1, unvisited((x - 1, y, West))))
        case (x, y, North) if unvisited.contains((x, y - 1, North)) =>
          unvisited.update((x, y - 1, North), math.min(currentNode._2 + 1, unvisited((x, y - 1, North))))
        case (x, y, South) if unvisited.contains((x, y + 1, South)) =>
          unvisited.update((x, y + 1, South), math.min(currentNode._2 + 1, unvisited((x, y + 1, South))))
        case _                                                      => ()
      // clockwise
      currentNode._1 match
        case (x, y, East) if unvisited.contains((x, y, South)) =>
          unvisited.update((x, y, South), math.min(currentNode._2 + 1000, unvisited((x, y, South))))
        case (x, y, West) if unvisited.contains((x, y, North)) =>
          unvisited.update((x, y, North), math.min(currentNode._2 + 1000, unvisited((x, y, North))))
        case (x, y, North) if unvisited.contains((x, y, East)) =>
          unvisited.update((x, y, East), math.min(currentNode._2 + 1000, unvisited((x, y, East))))
        case (x, y, South) if unvisited.contains((x, y, West)) =>
          unvisited.update((x, y, West), math.min(currentNode._2 + 1000, unvisited((x, y, West))))
        case _                                                 => ()
      // counterclockwise
      currentNode._1 match
        case (x, y, East) if unvisited.contains((x, y, North)) =>
          unvisited.update((x, y, North), math.min(currentNode._2 + 1000, unvisited((x, y, North))))
        case (x, y, West) if unvisited.contains((x, y, South)) =>
          unvisited.update((x, y, South), math.min(currentNode._2 + 1000, unvisited((x, y, South))))
        case (x, y, North) if unvisited.contains((x, y, West)) =>
          unvisited.update((x, y, West), math.min(currentNode._2 + 1000, unvisited((x, y, West))))
        case (x, y, South) if unvisited.contains((x, y, East)) =>
          unvisited.update((x, y, East), math.min(currentNode._2 + 1000, unvisited((x, y, East))))
        case _                                                 => ()
      unvisited.remove(currentNode._1)
      visited.update(currentNode._1, currentNode._2)
    val end     = map.find(_._2 == 'E')
    visited.collect:
      case p if p._1._1 == end.get._1._1 && p._1._2 == end.get._1._2 => p
    .minBy(_._2)._2

  def parse(line: String, y: Int): Map[(Int, Int), Char] =
    line.zipWithIndex.collect:
      case (char, x) => (x, y) -> char
    .toMap

  def handleLines(lines: List[String]): Map[(Int, Int), Char] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toMap

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day16
