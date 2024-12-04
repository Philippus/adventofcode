package adventofcode2024

import scala.io.Source
import scala.util.Using

object Day04:
  case class Pos(y: Int, x: Int)

  var grid: Array[Array[Char]] = Array.fill(140)(Array.fill(140)('.'))

  def initializeGrid(lineWithIndex: (String, Int)): Unit =
    for
      x <- 0.until(lineWithIndex._1.length)
    yield grid(lineWithIndex._2)(x) = lineWithIndex._1.charAt(x)

  def findChar(grid: Array[Array[Char]], char: Char): Seq[Pos] =
    for
      x <- grid.indices
      y <- grid.head.indices
      if grid(x)(y) == char
    yield Pos(x, y)

  def countXmas(grid: Array[Array[Char]]): Int =
    val xs    = findChar(grid, 'X')
    val ms    = findChar(grid, 'M')
    val as    = findChar(grid, 'A')
    val ss    = findChar(grid, 'S')
    var count = 0
    xs.foreach { x =>
      if (ms.contains(Pos(x.y, x.x - 1)) && as.contains(Pos(x.y, x.x - 2)) && ss.contains(Pos(x.y, x.x - 3))) count += 1
      if (ms.contains(Pos(x.y, x.x + 1)) && as.contains(Pos(x.y, x.x + 2)) && ss.contains(Pos(x.y, x.x + 3))) count += 1
      if (ms.contains(Pos(x.y - 1, x.x)) && as.contains(Pos(x.y - 2, x.x)) && ss.contains(Pos(x.y - 3, x.x))) count += 1
      if (ms.contains(Pos(x.y + 1, x.x)) && as.contains(Pos(x.y + 2, x.x)) && ss.contains(Pos(x.y + 3, x.x))) count += 1
      if (
        ms.contains(Pos(x.y + 1, x.x - 1)) && as.contains(Pos(x.y + 2, x.x - 2)) && ss.contains(Pos(x.y + 3, x.x - 3))
      ) count += 1
      if (
        ms.contains(Pos(x.y + 1, x.x + 1)) && as.contains(Pos(x.y + 2, x.x + 2)) && ss.contains(Pos(x.y + 3, x.x + 3))
      ) count += 1
      if (
        ms.contains(Pos(x.y - 1, x.x - 1)) && as.contains(Pos(x.y - 2, x.x - 2)) && ss.contains(Pos(x.y - 3, x.x - 3))
      ) count += 1
      if (
        ms.contains(Pos(x.y - 1, x.x + 1)) && as.contains(Pos(x.y - 2, x.x + 2)) && ss.contains(Pos(x.y - 3, x.x + 3))
      ) count += 1
    }
    count

  def countMas(grid: Array[Array[Char]]): Int =
    val ms    = findChar(grid, 'M')
    val as    = findChar(grid, 'A')
    val ss    = findChar(grid, 'S')
    var count = 0
    as.foreach { a =>
      if (
        (ms.contains(Pos(a.y - 1, a.x - 1)) && ss.contains(Pos(a.y + 1, a.x + 1)) && ms.contains(
          Pos(a.y + 1, a.x - 1)
        ) && ss.contains(Pos(a.y - 1, a.x + 1))) ||
        (ss.contains(Pos(a.y - 1, a.x - 1)) && ms.contains(Pos(a.y + 1, a.x + 1)) && ss.contains(
          Pos(a.y + 1, a.x - 1)
        ) && ms.contains(Pos(a.y - 1, a.x + 1))) ||
        (ms.contains(Pos(a.y - 1, a.x - 1)) && ss.contains(Pos(a.y + 1, a.x + 1)) && ss.contains(
          Pos(a.y + 1, a.x - 1)
        ) && ms.contains(Pos(a.y - 1, a.x + 1))) ||
        (ss.contains(Pos(a.y - 1, a.x - 1)) && ms.contains(Pos(a.y + 1, a.x + 1)) && ms.contains(
          Pos(a.y + 1, a.x - 1)
        ) && ss.contains(Pos(a.y - 1, a.x + 1)))
      ) count += 1
    }
    count

  def importLines(): Unit =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq.zipWithIndex.map(initializeGrid)
end Day04
