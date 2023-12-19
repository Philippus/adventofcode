package adventofcode2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10:
  case class Pos(y: Int, x: Int)

  var grid: Array[Array[(Char, Int)]] = Array.fill(140)(Array.fill(140)(('.', -1)))

  def nextPos(grid: Array[Array[(Char, Int)]], pos: Pos): Option[Pos] =
    val ns = grid(pos.y)(pos.x)._1 match
      case '|' => Seq(Pos(pos.y - 1, pos.x), Pos(pos.y + 1, pos.x))
      case '-' => Seq(Pos(pos.y, pos.x - 1), Pos(pos.y, pos.x + 1))
      case 'L' => Seq(Pos(pos.y - 1, pos.x), Pos(pos.y, pos.x + 1))
      case 'J' => Seq(Pos(pos.y, pos.x - 1), Pos(pos.y - 1, pos.x))
      case '7' => Seq(Pos(pos.y + 1, pos.x), Pos(pos.y, pos.x - 1))
      case 'F' => Seq(Pos(pos.y + 1, pos.x), Pos(pos.y, pos.x + 1))
      case '.' => Seq.empty
      case 'S' => Seq.empty
    ns.find(n => grid(n.y)(n.x)._2 == -1)

  def findStartPos(grid: Array[Array[(Char, Int)]]): Pos =
    (for
      x <- grid.indices
      y <- grid.indices
      if grid(x)(y) == ('S', -1)
    yield Pos(x, y)).head

  def findMax(grid: Array[Array[(Char, Int)]]): Int =
    (for
      x <- grid.indices
      y <- grid.indices
    yield grid(y)(x)._2).max

  def findMaxInBigGrid(): Int =
    importLines()
    val startPos   = findStartPos(grid)
    grid(startPos.y)(startPos.x) = ('7', 0)
    var currentPos = startPos
    var done       = false
    while (!done)
      nextPos(grid, currentPos) match
        case Some(value) =>
          grid(value.y)(value.x) = (grid(value.y)(value.x)._1, grid(currentPos.y)(currentPos.x)._2 + 1)
          currentPos = value
        case None        => done = true
    (findMax(grid) + 1) / 2

  def initializeGrid(lineWithIndex: (String, Int)): Unit =
    for
      x <- 0.until(lineWithIndex._1.length)
    yield grid(lineWithIndex._2)(x) = (lineWithIndex._1.charAt(x), -1)

  def importLines(): Unit =
    Using.resource(Source.fromResource("2023/day10input.txt")): source =>
      source.getLines().toSeq.zipWithIndex.map(initializeGrid)
end Day10
