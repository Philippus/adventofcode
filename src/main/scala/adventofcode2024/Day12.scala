package adventofcode2024

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day12:
  case class Region(i: Int, c: Char)
  case class Pos(x: Int, y: Int):
    def north: Pos = Pos(x, y - 1)
    def east: Pos  = Pos(x + 1, y)
    def south: Pos = Pos(x, y + 1)
    def west: Pos  = Pos(x - 1, y)
  end Pos

  def totalPriceOfFencingAllRegions(map: Map[Pos, Char]): Long =
    val unvisitedAll = map.to(mutable.Map)
    val visited      = mutable.Map[Pos, (Region, Long)]()
    var regionI      = -1
    while unvisitedAll.nonEmpty do
      val currentPos = unvisitedAll.head
      regionI += 1
      val region     = Region(regionI, currentPos._2)
      val queue      = mutable.ListBuffer(currentPos)
      while queue.nonEmpty do
        val currentPos = queue.head
        var fences     = 0
        if map.getOrElse(currentPos._1.north, '-') != currentPos._2 then
          fences += 1
        else if !visited.exists(_._1 == currentPos._1.north) && !queue.exists(_._1 == currentPos._1.north) then
          queue.addOne(currentPos._1.north, currentPos._2)
        if map.getOrElse(currentPos._1.east, '-') != currentPos._2 then
          fences += 1
        else if !visited.exists(_._1 == currentPos._1.east) && !queue.exists(_._1 == currentPos._1.east) then
          queue.addOne(currentPos._1.east, currentPos._2)
        if map.getOrElse(currentPos._1.south, '-') != currentPos._2 then
          fences += 1
        else if !visited.exists(_._1 == currentPos._1.south) && !queue.exists(_._1 == currentPos._1.south) then
          queue.addOne(currentPos._1.south, currentPos._2)
        if map.getOrElse(currentPos._1.west, '-') != currentPos._2 then
          fences += 1
        else if !visited.exists(_._1 == currentPos._1.west) && !queue.exists(_._1 == currentPos._1.west) then
          queue.addOne(currentPos._1.west, currentPos._2)
        visited.update(currentPos._1, (region, fences))
        unvisitedAll.remove(currentPos._1)
        queue.remove(0)
    visited.groupBy(_._2._1.i).map(i => i._2.size * i._2.map(_._2._2).sum).sum

  def parse(line: String, y: Int): Map[Pos, Char] =
    line.zipWithIndex.collect:
      case (char, x) => Pos(x, y) -> char
    .toMap

  def handleLines(lines: List[String]): Map[Pos, Char] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toMap

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day12
