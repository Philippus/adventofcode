package adventofcode2025

import scala.io.Source
import scala.util.Using

object Day04:
  def countAdjacentRolls(grid: Array[Array[Char]], pos: (x: Int, y: Int)): Int =
    val adjacentRolls =
      for
        cy       <- pos.y - 1 to pos.y + 1
        cx       <- pos.x - 1 to pos.x + 1
        if (cx, cy) != (pos.x, pos.y) // exclude given position
        if cy >= 0 && cy < grid.length && cx >= 0 && cx < grid(cy).length // exclude out of bounds positions
        candidate = grid(cy)(cx)
        if candidate == '@' || candidate == 'x'
      yield candidate
    adjacentRolls.length

  def countAccessibleRolls(grid: Array[Array[Char]]): Int =
    (for
      y <- grid.indices
      x <- grid(y).indices
    yield if grid(y)(x) == '@' && countAdjacentRolls(grid, (x, y)) < 4 then 1 else 0).sum

  def countAccessibleRollsAndUpdateGrid(grid: Array[Array[Char]]): Int =
    var count = 0
    for
      y <- grid.indices
      x <- grid(y).indices
      if grid(y)(x) == '@' && countAdjacentRolls(grid, (x, y)) < 4
    do
      count += 1
      grid(y)(x) = 'x'
    count

  def countRemovableRolls(grid: Array[Array[Char]]): Int =
    var count = 0
    var done  = false
    while !done do
      val accessible = countAccessibleRollsAndUpdateGrid(grid)
      if accessible == 0 then
        done = true
      else
        count += accessible
        for
          y <- grid.indices
          x <- grid(y).indices
          if grid(y)(x) == 'x'
        do
          grid(y)(x) = '.'
    count

  def drawGrid(grid: Array[Array[Char]]): String =
    grid.map(_.mkString).mkString("\n") :+ '\n'

  def parse(input: String): Array[Array[Char]] =
    input.split('\n').map(_.toCharArray)

  def importLines(): String =
    Using.resource(Source.fromResource("2025/day04input.txt")): source =>
      source.mkString
end Day04
