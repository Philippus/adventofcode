package adventofcode2025

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day07:
  def calculateSplits(grid: Array[Array[Char]]): Int =
    var splits = 0
    for
      y <- grid.indices
      x <- grid(y).indices
      if y < grid.length - 1
    do
      if grid(y)(x) == '|' || grid(y)(x) == 'S' then
        if grid(y + 1)(x) == '^' then
          splits += 1
          grid(y + 1)(x - 1) = '|'
          grid(y + 1)(x + 1) = '|'
        else
          grid(y + 1)(x) = '|'
    splits

  def calculateTimelines(grid: Array[Array[Char]]): Long =
    val timelinesGrid: Array[Array[Long]] = Array.fill(grid.length, grid(0).length)(0)
    for
      y <- grid.indices
      x <- grid(y).indices
      if y < grid.length - 1
    do
      if grid(y)(x) == 'S' then
        timelinesGrid(y)(x) = 1
      if grid(y)(x) == '|' || grid(y)(x) == 'S' then
        if grid(y + 1)(x) == '^' then
          grid(y + 1)(x - 1) = '|'
          timelinesGrid(y + 1)(x - 1) = timelinesGrid(y + 1)(x - 1) + timelinesGrid(y)(x)
          grid(y + 1)(x + 1) = '|'
          timelinesGrid(y + 1)(x + 1) = timelinesGrid(y + 1)(x + 1) + timelinesGrid(y)(x)
        else
          grid(y + 1)(x) = '|'
          timelinesGrid(y + 1)(x) = timelinesGrid(y + 1)(x) + timelinesGrid(y)(x)
    timelinesGrid(grid.length - 1).sum

  def parse(input: String): Array[Array[Char]] =
    input.split('\n').map(_.toCharArray)

  def importLines(): String =
    Using.resource(Source.fromResource("2025/day07input.txt")): source =>
      source.mkString
end Day07
