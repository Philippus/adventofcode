package adventofcode2023

import scala.io.Source
import scala.util.Using

object Day14:
  def tiltNorth(platform: Array[Array[Char]]): Array[Array[Char]] =
    var changed = true
    while changed do
      changed = false
      for
        y <- 1.until(platform.length)
        x <- platform.indices
        if platform(y)(x) == 'O' && platform(y - 1)(x) == '.'
      do
        changed = true
        platform(y)(x) = '.'
        platform(y - 1)(x) = 'O'
    platform

  def tiltEast(platform: Array[Array[Char]]): Array[Array[Char]] =
    val reversed = platform.map(_.reverse)
    var changed  = true
    while changed do
      changed = false
      for
        y <- reversed.indices
        x <- 1.until(reversed.length)
        if reversed(y)(x) == 'O' && reversed(y)(x - 1) == '.'
      do
        changed = true
        reversed(y)(x) = '.'
        reversed(y)(x - 1) = 'O'
    reversed.map(_.reverse)

  def tiltSouth(platform: Array[Array[Char]]): Array[Array[Char]] =
    val reversed = platform.reverse
    var changed  = true
    while changed do
      changed = false
      for
        y <- 1.until(reversed.length)
        x <- reversed.indices
        if reversed(y)(x) == 'O' && reversed(y - 1)(x) == '.'
      do
        changed = true
        reversed(y)(x) = '.'
        reversed(y - 1)(x) = 'O'
    reversed.reverse

  def tiltWest(platform: Array[Array[Char]]): Array[Array[Char]] =
    var changed = true
    while changed do
      changed = false
      for
        y <- platform.indices
        x <- 1.until(platform.length)
        if platform(y)(x) == 'O' && platform(y)(x - 1) == '.'
      do
        changed = true
        platform(y)(x) = '.'
        platform(y)(x - 1) = 'O'
    platform

  def cycle(platform: Array[Array[Char]]): Array[Array[Char]] =
    tiltEast(tiltSouth(tiltWest(tiltNorth(platform))))

  def calculateTotalLoad(platform: Array[Array[Char]]): Int =
    platform.reverse.zipWithIndex.map: (row, i) =>
      row.count(_.==('O')) * (i + 1)
    .sum

  def tiltNorthAndCalculateLoad(platform: Array[Array[Char]]): Int =
    calculateTotalLoad(tiltNorth(platform))

  def cycle1000000000Times(platform: Array[Array[Char]]): Int =
    var p          = platform
    val seenBefore = scala.collection.mutable.ListBuffer[List[List[Char]]]()
    var result     = -1
    while result == -1 do
      p = cycle(p)
      if seenBefore.contains(p.map(_.toList).toList) then
        val firstIdx = seenBefore.indexOf(p.map(_.toList).toList)
        val period   = seenBefore.length - firstIdx
        val offset   = 1000000000 % period
        if (period + offset - 1) < seenBefore.length then
          result = calculateTotalLoad(seenBefore(period + offset - 1).map(_.toArray).toArray)
      seenBefore.addOne(p.map(_.toList).toList)
    result

  def drawPlatform(platform: Array[Array[Char]]): String =
    platform.map(_.mkString).mkString("\n") :+ '\n'

  def parse(input: String): Array[Array[Char]] =
    input.split("\n").map(_.toCharArray)

  def importLines(): String =
    Using.resource(Source.fromResource("2023/day14input.txt")): source =>
      source.mkString
end Day14
