package adventofcode2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day18:
  case class Pos(x: Int, y: Int):
    def north: Pos = Pos(x, y - 1)
    def ne: Pos    = Pos(x + 1, y - 1)
    def east: Pos  = Pos(x + 1, y)
    def se: Pos    = Pos(x + 1, y + 1)
    def south: Pos = Pos(x, y + 1)
    def sw: Pos    = Pos(x - 1, y + 1)
    def west: Pos  = Pos(x - 1, y)
    def nw: Pos    = Pos(x - 1, y - 1)

    def adjacent: Vector[Pos] =
      Vector(this.north, this.ne, this.east, this.se, this.south, this.sw, this.west, this.nw)
  end Pos

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(map: Map[Pos, Char]): String =
    createGrid(map.maxBy(_._1.x)._1.x + 1, map.maxBy(_._1.y)._1.y + 1).map:
      case (x, y) => (if x == 0 then "\n" else "") + map(Pos(x, y))
    .mkString

  def magicForest(map: Map[Pos, Char], minutes: Int): Map[Pos, Char] =
    def withinBounds(p: Pos): Boolean =
      p.x >= 0 && p.x <= map.maxBy(_._1.x)._1.x && p.y >= 0 && p.y <= map.maxBy(_._1.y)._1.y

    @tailrec
    def loop(map: Map[Pos, Char], minute: Int): Map[Pos, Char] =
      if minutes == minute then
        map
      else
        val newMap = map.map:
          case (p, c) if c == '.' =>
            if p.adjacent.filter(withinBounds).count(p => map(p) == '|') >= 3 then (p, '|') else (p, c)
          case (p, c) if c == '|' =>
            if p.adjacent.filter(withinBounds).count(p => map(p) == '#') >= 3 then (p, '#') else (p, c)
          case (p, c) if c == '#' =>
            if p.adjacent.filter(withinBounds).count(p => map(p) == '#') >= 1 && p.adjacent.filter(withinBounds).count(
                p => map(p) == '|'
              ) >= 1
            then (p, '#')
            else (p, '.')
        loop(newMap, minute + 1)

    loop(map, 0)

  def resourceValue(map: Map[Pos, Char], minutes: Int): Int =
    val newMap = magicForest(map, minutes)
    newMap.count(_._2 == '|') * newMap.count(_._2 == '#')

  def detectCycle(map: Map[Pos, Char]): Int =
    def withinBounds(p: Pos): Boolean =
      p.x >= 0 && p.x <= map.maxBy(_._1.x)._1.x && p.y >= 0 && p.y <= map.maxBy(_._1.y)._1.y

    @tailrec
    def loop(map: Map[Pos, Char], minute: Int, seen: Vector[Int]): Int =
      if seen.size >= 20 && seen.dropRight(10).containsSlice(seen.takeRight(10)) then
        seen.indexOfSlice(seen.takeRight(10)) - 1
      else
        val newMap = map.map:
          case (p, c) if c == '.' =>
            if p.adjacent.filter(withinBounds).count(p => map(p) == '|') >= 3 then (p, '|') else (p, c)
          case (p, c) if c == '|' =>
            if p.adjacent.filter(withinBounds).count(p => map(p) == '#') >= 3 then (p, '#') else (p, c)
          case (p, c) if c == '#' =>
            if p.adjacent.filter(withinBounds).count(p => map(p) == '#') >= 1 && p.adjacent.filter(withinBounds).count(
                p => map(p) == '|'
              ) >= 1
            then (p, '#')
            else (p, '.')
        loop(newMap, minute + 1, seen :+ (newMap.count(_._2 == '|') * newMap.count(_._2 == '#')))

    loop(map, 0, Vector.empty[Int])

  def parse(line: String, y: Int): Map[Pos, Char] =
    line.zipWithIndex.collect:
      case (char, x) => Pos(x, y) -> char
    .toMap

  def handleLines(lines: Vector[String]): Map[Pos, Char] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toMap

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day18
