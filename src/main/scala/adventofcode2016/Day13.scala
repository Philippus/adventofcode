package adventofcode2016

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day13:
  case class Pos(x: Int, y: Int):
    def north: Pos = Pos(x, y - 1)
    def east: Pos  = Pos(x + 1, y)
    def south: Pos = Pos(x, y + 1)
    def west: Pos  = Pos(x - 1, y)
  end Pos

  def isWall(pos: Pos, favouriteNumber: Int): Boolean =
    if pos.x < 0 || pos.y < 0 then
      true
    else
      val (x, y) = (pos.x, pos.y)
      (x * x + 3 * x + 2 * x * y + y + y * y + favouriteNumber).toBinaryString.count(_.==('1')) % 2 == 1

  def fewestStepsToCubicle(start: Pos, goal: Pos, favouriteNumber: Int): Long =
    val unvisited: mutable.Map[Pos, Long] = Map(start -> 0L).to(mutable.Map)

    val visited = mutable.Map[Pos, Long]()

    var lowest = Long.MaxValue
    while lowest == Long.MaxValue do
      val (currentNode, currentVal) = unvisited.minBy(_._2)
      if currentNode == goal then
        lowest = currentVal
      if !isWall(currentNode.north, favouriteNumber) && !visited.contains(currentNode.north) then
        unvisited.update(
          currentNode.north,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.north, Long.MaxValue))
        )
      if !isWall(currentNode.east, favouriteNumber) && !visited.contains(currentNode.east) then
        unvisited.update(
          currentNode.east,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.east, Long.MaxValue))
        )
      if !isWall(currentNode.south, favouriteNumber) && !visited.contains(currentNode.south) then
        unvisited.update(
          currentNode.south,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.south, Long.MaxValue))
        )
      if !isWall(currentNode.west, favouriteNumber) && !visited.contains(currentNode.west) then
        unvisited.update(
          currentNode.west,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.west, Long.MaxValue))
        )

      unvisited.remove(currentNode)
      visited.update(currentNode, currentVal)

    lowest

  def reachableInAtMost50Steps(start: Pos, goal: Pos, favouriteNumber: Int): Long =
    val unvisited: mutable.Map[Pos, Long] = Map(start -> 0L).to(mutable.Map)

    val visited = mutable.Map[Pos, Long]()

    while unvisited.nonEmpty do
      val (currentNode, currentVal) = unvisited.minBy(_._2)
      if !isWall(currentNode.north, favouriteNumber) && !visited.contains(currentNode.north) then
        unvisited.update(
          currentNode.north,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.north, Long.MaxValue))
        )
      if !isWall(currentNode.east, favouriteNumber) && !visited.contains(currentNode.east) then
        unvisited.update(
          currentNode.east,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.east, Long.MaxValue))
        )
      if !isWall(currentNode.south, favouriteNumber) && !visited.contains(currentNode.south) then
        unvisited.update(
          currentNode.south,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.south, Long.MaxValue))
        )
      if !isWall(currentNode.west, favouriteNumber) && !visited.contains(currentNode.west) then
        unvisited.update(
          currentNode.west,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.west, Long.MaxValue))
        )

      unvisited.remove(currentNode)
      visited.update(currentNode, currentVal)

    visited.count(_._2 <= 50)

  def importLines(): Int =
    Using.resource(Source.fromResource(s"2016/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next().toInt
end Day13
