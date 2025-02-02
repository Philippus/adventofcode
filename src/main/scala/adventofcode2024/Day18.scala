package adventofcode2024

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day18:
  case class Pos(x: Int, y: Int):
    def up: Pos    = Pos(x, y - 1)
    def right: Pos = Pos(x + 1, y)
    def down: Pos  = Pos(x, y + 1)
    def left: Pos  = Pos(x - 1, y)
  end Pos

  def minimumStepsToExit(coordinates: Vector[Pos], endPos: Pos, fallenBytes: Int): Long =
    def mapAfterCorruption(fallenBytes: Int, coordinates: Vector[Pos]): Map[Pos, Char] =
      coordinates.take(fallenBytes).map: pos =>
        pos -> '#'
      .toMap

    val map = mapAfterCorruption(fallenBytes, coordinates)

    val unvisited: mutable.Map[Pos, Long] =
      (for
        x <- 0 to endPos.x
        y <- 0 to endPos.y
      yield
        if x == 0 && y == 0 then Pos(x, y) -> 0L
        else Pos(x, y)                     -> Long.MaxValue)
        .to(mutable.Map)

    val visited = mutable.Map[Pos, Long]()

    while unvisited.exists(_._2 < Long.MaxValue) do
      val (currentNode, currentVal) = unvisited.minBy(_._2)
      if !map.contains(currentNode.up) && unvisited.contains(currentNode.up) then
        unvisited.update(currentNode.up, math.min(currentVal + 1, unvisited.getOrElse(currentNode.up, Long.MaxValue)))
      if !map.contains(currentNode.down) && unvisited.contains(currentNode.down) then
        unvisited.update(
          currentNode.down,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.down, Long.MaxValue))
        )
      if !map.contains(currentNode.right) && unvisited.contains(currentNode.right) then
        unvisited.update(
          currentNode.right,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.right, Long.MaxValue))
        )
      if !map.contains(currentNode.left) && unvisited.contains(currentNode.left) then
        unvisited.update(
          currentNode.left,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.left, Long.MaxValue))
        )

      unvisited.remove(currentNode)
      visited.update(currentNode, currentVal)

    visited(endPos)

  def fallenByteBlocksExit(coordinates: Vector[Pos], endPos: Pos, fallenBytes: Int): Boolean =
    def mapAfterCorruption(fallenBytes: Int, coordinates: Vector[Pos]): Map[Pos, Char] =
      coordinates.take(fallenBytes).map: pos =>
        pos -> '#'
      .toMap

    val map = mapAfterCorruption(fallenBytes, coordinates)

    val unvisited: mutable.Map[Pos, Long] =
      (for
        x <- 0 to endPos.x
        y <- 0 to endPos.y
      yield
        if x == 0 && y == 0 then Pos(x, y) -> 0L
        else Pos(x, y)                     -> Long.MaxValue)
        .to(mutable.Map)

    val visited = mutable.Map[Pos, Long]()

    while unvisited.exists(_._2 < Long.MaxValue) do
      val (currentNode, currentVal) = unvisited.minBy(_._2)
      if !map.contains(currentNode.up) && unvisited.contains(currentNode.up) then
        unvisited.update(currentNode.up, math.min(currentVal + 1, unvisited.getOrElse(currentNode.up, Long.MaxValue)))
      if !map.contains(currentNode.down) && unvisited.contains(currentNode.down) then
        unvisited.update(
          currentNode.down,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.down, Long.MaxValue))
        )
      if !map.contains(currentNode.right) && unvisited.contains(currentNode.right) then
        unvisited.update(
          currentNode.right,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.right, Long.MaxValue))
        )
      if !map.contains(currentNode.left) && unvisited.contains(currentNode.left) then
        unvisited.update(
          currentNode.left,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.left, Long.MaxValue))
        )

      unvisited.remove(currentNode)
      visited.update(currentNode, currentVal)
    !visited.exists(_._1 == endPos)

  def findFirstFallenByteBlockingExit(coordinates: Vector[Pos], endPos: Pos): String =
    var fallenBytes = coordinates.length
    while fallenByteBlocksExit(coordinates, endPos, fallenBytes) do
      fallenBytes -= 1
    s"${coordinates(fallenBytes).x},${coordinates(fallenBytes).y}"

  def handleLines(lines: Vector[String]): Vector[Pos] =
    lines.map:
      case s"$x,$y" => Pos(x.toInt, y.toInt)

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day18
