package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

import adventofcode2021.Day13.Fold.*

object Day13:
  case class Pos(x: Int, y: Int)

  enum Fold:
    case FoldX(x: Int) extends Fold
    case FoldY(y: Int) extends Fold

  def countDots(grid: Array[Array[Char]]): Int =
    grid.map(_.mkString).mkString.count(_.==('#'))

  def fold(grid: Array[Array[Char]], fold: Fold): Array[Array[Char]] =
    val (reversedGrid, newGrid) = fold match
      case FoldY(y) =>
        (grid.reverse, Array.fill(y)(Array.fill(grid(0).length)('.')))
      case FoldX(x) =>
        (grid.map(_.reverse), Array.fill(grid.length)(Array.fill(x)('.')))
    for
      y <- newGrid.indices
      x <- newGrid(y).indices
      if grid(y)(x) == '#' || reversedGrid(y)(x) == '#'
    do
      newGrid(y)(x) = '#'
    newGrid

  def foldPaperOnceAndCountDots(positions: List[Pos], folds: List[Fold]): Int =
    countDots(fold(positionsToGrid(positions, folds), folds.head))

  def foldAll(grid: Array[Array[Char]], folds: List[Fold]): Array[Array[Char]] =
    @tailrec
    def loop(folds: List[Fold], grid: Array[Array[Char]]): Array[Array[Char]] =
      if folds.isEmpty then
        grid
      else
        loop(folds.tail, fold(grid, folds.head))
    loop(folds, grid)

  def positionsToGrid(positions: List[Pos], folds: List[Fold]): Array[Array[Char]] =
    val length = folds.collectFirst:
      case FoldY(y) => y * 2 + 1
    .get
    val width  = folds.collectFirst:
      case FoldX(x) => x * 2 + 1
    .get
    val grid   = Array.fill(length)(Array.fill(width)('.'))
    for
      pos <- positions
    do
      grid(pos.y)(pos.x) = '#'
    grid

  def drawGrid(grid: Array[Array[Char]]): String =
    grid.map(_.mkString).mkString("\n") :+ '\n'

  def drawsGridAfterFoldingPaper(positions: List[Pos], folds: List[Fold]): String =
    drawGrid(foldAll(positionsToGrid(positions, folds), folds))

  def parse(input: String): (List[Pos], List[Fold]) =
    val split     = input.split("\n\n")
    val positions = split.head.split("\n").map:
      case s"$x,$y" => Pos(x.toInt, y.toInt)
    val folds     = split.last.split("\n").map:
      case s"fold along x=$x" => FoldX(x.toInt)
      case s"fold along y=$y" => FoldY(y.toInt)
    (positions.toList, folds.toList)

  def importLines(): String =
    Using.resource(Source.fromResource(s"2021/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.mkString
end Day13
