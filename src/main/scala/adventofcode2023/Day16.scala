package adventofcode2023

import scala.io.Source
import scala.util.Using

import adventofcode2023.Day16.Direction.*

object Day16:
  case class Pos(x: Int, y: Int):
    def north: Pos = this.copy(y = this.y - 1)
    def east: Pos  = this.copy(x = this.x + 1)
    def south: Pos = this.copy(y = this.y + 1)
    def west: Pos  = this.copy(x = this.x - 1)

  enum Direction:
    case North, East, South, West

  case class Cursor(pos: Pos, direction: Direction)

  def energizedTiles(grid: Array[Array[Char]], start: Cursor): Int =
    val visited = scala.collection.mutable.ListBuffer[Cursor]()
    val queue   = scala.collection.mutable.ListBuffer[Cursor](start)
    while queue.nonEmpty do
      val cursor = queue.head
      cursor match
        case Cursor(pos, North) =>
          grid(pos.y)(pos.x) match
            case '-'  =>
              if pos.x + 1 < grid(0).length && !visited.contains(cursor.copy(pos = cursor.pos.east, direction = East))
              then queue.addOne(cursor.copy(pos = cursor.pos.east, direction = East))
              if pos.x - 1 >= 0 && !visited.contains(cursor.copy(pos = cursor.pos.west, direction = West)) then
                queue.addOne(cursor.copy(pos = cursor.pos.west, direction = West))
            case '/'  =>
              if pos.x + 1 < grid(0).length && !visited.contains(cursor.copy(pos = cursor.pos.east, direction = East))
              then queue.addOne(cursor.copy(pos = cursor.pos.east, direction = East))
            case '\\' =>
              if pos.x - 1 >= 0 && !visited.contains(cursor.copy(pos = cursor.pos.west, direction = West)) then
                queue.addOne(cursor.copy(pos = cursor.pos.west, direction = West))
            case _    =>
              if pos.y - 1 >= 0 && !visited.contains(cursor.copy(pos = cursor.pos.north)) then
                queue.addOne(cursor.copy(pos = cursor.pos.north))
        case Cursor(pos, East)  =>
          grid(pos.y)(pos.x) match
            case '|'  =>
              if pos.y + 1 < grid.length && !visited.contains(cursor.copy(cursor.pos.south, direction = South)) then
                queue.addOne(cursor.copy(pos = cursor.pos.south, direction = South))
              if pos.y - 1 >= 0 && !visited.contains(cursor.copy(cursor.pos.north, direction = North)) then
                queue.addOne(cursor.copy(pos = cursor.pos.north, direction = North))
            case '/'  =>
              if pos.y - 1 >= 0 && !visited.contains(cursor.copy(cursor.pos.north, direction = North)) then
                queue.addOne(cursor.copy(pos = cursor.pos.north, direction = North))
            case '\\' =>
              if pos.y + 1 < grid.length && !visited.contains(cursor.copy(cursor.pos.south, direction = South)) then
                queue.addOne(cursor.copy(pos = cursor.pos.south, direction = South))
            case _    =>
              if pos.x + 1 < grid(0).length && !visited.contains(cursor.copy(pos = cursor.pos.east, direction = East))
              then queue.addOne(cursor.copy(pos = cursor.pos.east))
        case Cursor(pos, South) =>
          grid(pos.y)(pos.x) match
            case '-'  =>
              if pos.x + 1 < grid(0).length && !visited.contains(cursor.copy(pos = cursor.pos.east, direction = East))
              then queue.addOne(cursor.copy(pos = cursor.pos.east, direction = East))
              if pos.x - 1 >= 0 && !visited.contains(cursor.copy(pos = cursor.pos.west, direction = West)) then
                queue.addOne(cursor.copy(pos = cursor.pos.west, direction = West))
            case '/'  =>
              if pos.x - 1 >= 0 && !visited.contains(cursor.copy(pos = cursor.pos.west, direction = West)) then
                queue.addOne(cursor.copy(pos = cursor.pos.west, direction = West))
            case '\\' =>
              if pos.x + 1 < grid(0).length && !visited.contains(cursor.copy(pos = cursor.pos.east, direction = East))
              then queue.addOne(cursor.copy(pos = cursor.pos.east, direction = East))
            case _    =>
              if pos.y + 1 < grid.length && !visited.contains(cursor.copy(pos = cursor.pos.south)) then
                queue.addOne(cursor.copy(pos = cursor.pos.south))
        case Cursor(pos, West)  =>
          grid(pos.y)(pos.x) match
            case '|'  =>
              if pos.y + 1 < grid.length && !visited.contains(cursor.copy(cursor.pos.south, direction = South)) then
                queue.addOne(cursor.copy(pos = cursor.pos.south, direction = South))
              if pos.y - 1 > 0 && !visited.contains(cursor.copy(cursor.pos.north, direction = North)) then
                queue.addOne(cursor.copy(pos = cursor.pos.north, direction = North))
            case '/'  =>
              if pos.y + 1 < grid.length && !visited.contains(cursor.copy(cursor.pos.south, direction = South)) then
                queue.addOne(cursor.copy(pos = cursor.pos.south, direction = South))
            case '\\' =>
              if pos.y - 1 >= 0 && !visited.contains(cursor.copy(cursor.pos.north, direction = North)) then
                queue.addOne(cursor.copy(pos = cursor.pos.north, direction = North))
            case _    =>
              if pos.x - 1 >= 0 && !visited.contains(cursor.copy(pos = cursor.pos.west)) then
                queue.addOne(cursor.copy(pos = cursor.pos.west))
      visited.addOne(cursor)
      queue.remove(0)
    visited.map(_.pos).distinct.length
  end energizedTiles

  def energizedTilesFromTopLeftCornerHeadingRight(grid: Array[Array[Char]]): Int =
    energizedTiles(grid, Cursor(Pos(0, 0), East))

  def findConfigurationEnergizingLargestNumberOfTiles(grid: Array[Array[Char]]): Int =
    val headingDown =
      for
        x <- grid(0).indices
      yield energizedTiles(grid, Cursor(Pos(x, 0), South))

    val headingUp =
      for
        x <- grid(0).indices
      yield energizedTiles(grid, Cursor(Pos(x, grid.length - 1), North))

    val headingRight =
      for
        y <- grid.indices
      yield energizedTiles(grid, Cursor(Pos(0, y), East))

    val headingLeft =
      for
        y <- grid.indices
      yield energizedTiles(grid, Cursor(Pos(grid(0).length - 1, y), West))

    (headingDown ++ headingUp ++ headingRight ++ headingLeft).max

  def parse(input: String): Array[Array[Char]] =
    input.split("\n").map(_.toCharArray)

  def importLines(): String =
    Using.resource(Source.fromResource("2023/day16input.txt")): source =>
      source.mkString
end Day16
