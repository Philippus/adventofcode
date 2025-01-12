package adventofcode2017

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

import Day10.*

object Day14:
  case class Pos(x: Int, y: Int):
    def north: Pos = Pos(x, y - 1)
    def east: Pos  = Pos(x + 1, y)
    def south: Pos = Pos(x, y + 1)
    def west: Pos  = Pos(x - 1, y)
  end Pos

  def hashToBits(s: String): String =
    s.map(char =>
      val i = Integer.parseInt(char.toString, 16)
      val b = i.toBinaryString
      "0" * (4 - b.length) ++ b
    ).mkString

  def knotHash(str: String): String =
    hashToBits(Day10.processLengthsAsAscii(0.to(255), stringToLengthsAndSuffix(str)))

  def countUsedSquares(str: String): Int =
    0.to(127).map(i => knotHash(s"$str-$i")).map(c => c.count(_.==('1'))).sum

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(map: Map[Pos, Char]): String =
    createGrid(map.maxBy(_._1.x)._1.x + 1, map.maxBy(_._1.y)._1.y + 1).map:
      case (x, y) => (if x == 0 then "\n" else "") + map(Pos(x, y))
    .mkString

  def parse(line: String, y: Int): Map[Pos, Char] =
    line.zipWithIndex.collect:
      case (char, x) => Pos(x, y) -> char
    .toMap

  def createMapFromHash(str: String): Map[Pos, Char] =
    0.to(127).map(i => (knotHash(s"$str-$i"), i)).flatMap(l => parse(l._1, l._2)).toMap

  def findRegion(initialMap: mutable.Map[Pos, Char], pos: Pos): Seq[Pos] =
    val map = initialMap.to(mutable.Map)

    def loop(currPos: Pos, acc: Seq[Pos]): Seq[Pos] =
      if map.getOrElse(currPos, '0') == '0' then
        acc
      else
        map.update(currPos, '0')
        loop(currPos.north, acc :+ currPos) ++
          loop(currPos.east, acc :+ currPos) ++
          loop(currPos.south, acc :+ currPos) ++
          loop(currPos.west, acc :+ currPos)
    loop(pos, Seq.empty)

  def countRegions(str: String): Int =
    val map         = createMapFromHash(str).to(mutable.Map)
    var posOfRegion = map.find(_._2 == '1')
    var regions     = 0
    while posOfRegion.nonEmpty do
      regions += 1
      val positions = findRegion(map, posOfRegion.get._1)
      positions.foreach: pos =>
        map.update(pos, '0')
      posOfRegion = map.find(_._2 == '1')
    regions

  def importLines(): String =
    Using.resource(Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next()
end Day14
