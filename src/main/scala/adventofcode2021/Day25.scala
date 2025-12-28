package adventofcode2021

import scala.io.Source
import scala.util.Using

object Day25:
  def findStepOnWhichNoSeaCucumbersMove(grid: Array[Array[Char]]): Int =
    val xMax  = grid.head.length - 1
    val yMax  = grid.length - 1
    var steps = 0
    var done  = false
    while !done do
      steps += 1

      val eastFacing =
        for
          y    <- grid.indices
          x    <- grid(0).indices
          if grid(y)(x) == '>'
          nextX = if x == xMax then 0 else x + 1
          if grid(y)(nextX) == '.'
        yield (x, y, nextX, y)
      eastFacing.foreach: (x, y, nextX, nextY) =>
        grid(nextY)(nextX) = '>'
        grid(y)(x) = '.'

      val southFacing =
        for
          y    <- grid.indices
          x    <- grid(0).indices
          if grid(y)(x) == 'v'
          nextY = if y == yMax then 0 else y + 1
          if grid(nextY)(x) == '.'
        yield (x, y, x, nextY)
      southFacing.foreach: (x, y, nextX, nextY) =>
        grid(nextY)(nextX) = 'v'
        grid(y)(x) = '.'

      done = eastFacing.isEmpty && southFacing.isEmpty
    steps

  def parse(input: String): Array[Array[Char]] =
    input.split('\n').map(_.toCharArray)

  def importLines(): String =
    Using.resource(Source.fromResource("2021/day25input.txt")): source =>
      source.mkString
end Day25
