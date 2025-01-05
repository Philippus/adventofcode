package adventofcode2022

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day12:
  case class Pos(x: Int, y: Int):
    def north: Pos = Pos(x, y - 1)
    def east: Pos  = Pos(x + 1, y)
    def south: Pos = Pos(x, y + 1)
    def west: Pos  = Pos(x - 1, y)
  end Pos

  def cmpElevation(current: Char, next: Char): Boolean =
    (current, next) match
      case ('S', _) => next.toInt == 'a'.toInt || next.toInt == 'b'.toInt
      case (_, 'S') => current.toInt == 'a'.toInt || current.toInt == 'b'.toInt
      case ('E', _) => next.toInt <= 'z'.toInt
      case (_, 'E') => current.toInt == 'z'.toInt || current.toInt == 'z'.toInt - 1
      case _        => next.toInt <= current.toInt + 1

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(map: Map[Pos, Char], width: Int, height: Int): String =
    createGrid(width, height).map:
      case (x, y) => (if x == 0 then "\n" else "") + map(Pos(x, y))
    .mkString

  def fewestStepsToBestSignal(map: Map[Pos, Char]): Long =
    val unvisited: mutable.Map[Pos, Long] =
      map.map((p, c) => if c == 'S' then p -> 0L else p -> Long.MaxValue).to(mutable.Map)

    val visited = mutable.Map[Pos, Long]()

    while unvisited.exists(_._2 < Long.MaxValue) do
      val (currentNode, currentVal) = unvisited.minBy(_._2)
      if unvisited.contains(currentNode.north) && cmpElevation(map(currentNode), map(currentNode.north)) then
        unvisited.update(currentNode.north, math.min(currentVal + 1, unvisited(currentNode.north)))
      if unvisited.contains(currentNode.east) && cmpElevation(map(currentNode), map(currentNode.east)) then
        unvisited.update(currentNode.east, math.min(currentVal + 1, unvisited(currentNode.east)))
      if unvisited.contains(currentNode.south) && cmpElevation(map(currentNode), map(currentNode.south)) then
        unvisited.update(currentNode.south, math.min(currentVal + 1, unvisited(currentNode.south)))
      if unvisited.contains(currentNode.west) && cmpElevation(map(currentNode), map(currentNode.west)) then
        unvisited.update(currentNode.west, math.min(currentVal + 1, unvisited(currentNode.west)))

      unvisited.remove(currentNode)
      visited.update(currentNode, currentVal)

    val (endPos, _) = map.find(_._2 == 'E').get
    visited(endPos)

  def fewestStepsToBestStart(map: Map[Pos, Char]): Long =
    val unvisited: mutable.Map[Pos, Long] =
      map.map((p, c) => if c == 'E' then p -> 0L else p -> Long.MaxValue).to(mutable.Map)

    val visited   = mutable.Map[Pos, Long]()
    var lowestVal = Long.MaxValue
    while lowestVal == Long.MaxValue do
      val (currentNode, currentVal) = unvisited.minBy(_._2)
      if unvisited.contains(currentNode.north) && cmpElevation(map(currentNode.north), map(currentNode)) then
        unvisited.update(currentNode.north, math.min(currentVal + 1, unvisited(currentNode.north)))
      if unvisited.contains(currentNode.east) && cmpElevation(map(currentNode.east), map(currentNode)) then
        unvisited.update(currentNode.east, math.min(currentVal + 1, unvisited(currentNode.east)))
      if unvisited.contains(currentNode.south) && cmpElevation(map(currentNode.south), map(currentNode)) then
        unvisited.update(currentNode.south, math.min(currentVal + 1, unvisited(currentNode.south)))
      if unvisited.contains(currentNode.west) && cmpElevation(map(currentNode.west), map(currentNode)) then
        unvisited.update(currentNode.west, math.min(currentVal + 1, unvisited(currentNode.west)))

      unvisited.remove(currentNode)
      visited.update(currentNode, currentVal)

      if map(currentNode) == 'a' then
        lowestVal = currentVal
    lowestVal

  def parse(line: String, y: Int): Map[Pos, Char] =
    line.zipWithIndex.collect:
      case (char, x) => Pos(x, y) -> char
    .toMap

  def handleLines(lines: List[String]): Map[Pos, Char] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toMap

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day12
