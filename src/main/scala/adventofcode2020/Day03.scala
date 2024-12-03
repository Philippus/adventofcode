package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03:
  def countTrees(right: Int, down: Int, map: Vector[Vector[Char]]): Int =
    @tailrec
    def loop(row: Int, column: Int, acc: Int): Int =
      if row >= map.length then
        acc
      else if map(row)(column % map.head.length) == '#' then
        loop(row + down, column + right, acc + 1)
      else
        loop(row + down, column + right, acc)
    loop(0, 0, 0)

  def checkSlopes(slopes: Seq[(Int, Int)], map: Vector[Vector[Char]]): Long =
    slopes.map((r, d) => countTrees(r, d, map).toLong).product

  def handleLines(s: Seq[String]): Vector[Vector[Char]] =
    Vector(s.map(Vector(_*))*)

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2020/day03input.txt")):
      _.getLines().toSeq
end Day03
