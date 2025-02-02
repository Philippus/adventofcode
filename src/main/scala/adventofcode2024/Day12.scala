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

  def discountedTotalPriceOfFencingAllRegions(map: Map[Pos, Char]): Long =
    def costOfRegion(region: Vector[(Pos, (Long, Long, Long, Long))]): Long =
      val north  = region.filter(_._2._1 == 1L).sortBy(_._1._2).map(_._1)
      val sidesN = north.map:
        case pos if !north.contains(Pos(pos.x - 1, pos.y)) => 1L
        case _                                             => 0L
      .sum
      val east   = region.filter(_._2._2 == 1L).sortBy(_._1._2).map(_._1)
      val sidesE = east.map:
        case pos if !east.contains(Pos(pos.x, pos.y - 1)) => 1L
        case _                                            => 0L
      .sum
      val south  = region.filter(_._2._3 == 1L).sortBy(_._1._2).map(_._1)
      val sidesS = south.map:
        case pos if !south.contains(Pos(pos.x - 1, pos.y)) => 1L
        case _                                             => 0L
      .sum
      val west   = region.filter(_._2._4 == 1L).sortBy(_._1._2).map(_._1)
      val sidesW = west.map:
        case pos if !west.contains(Pos(pos.x, pos.y - 1)) => 1L
        case _                                            => 0L
      .sum
      (sidesN + sidesE + sidesS + sidesW) * region.size

    val unvisitedAll = map.to(mutable.Map)
    val visited      = mutable.Map[Pos, (Region, (Long, Long, Long, Long))]()
    var regionI      = -1
    var cost         = 0L
    while unvisitedAll.nonEmpty do
      val currentPos = unvisitedAll.head
      regionI += 1
      val region     = Region(regionI, currentPos._2)
      val queue      = mutable.ListBuffer(currentPos)
      val regionPoss = mutable.ListBuffer.empty[(Pos, (Long, Long, Long, Long))]
      while queue.nonEmpty do
        val currentPos = queue.head
        var fencesN    = 0L
        var fencesE    = 0L
        var fencesS    = 0L
        var fencesW    = 0L
        if map.getOrElse(currentPos._1.north, '-') != currentPos._2 then
          fencesN += 1
        else if !visited.exists(_._1 == currentPos._1.north) && !queue.exists(_._1 == currentPos._1.north) then
          queue.addOne(currentPos._1.north, currentPos._2)
        if map.getOrElse(currentPos._1.east, '-') != currentPos._2 then
          fencesE += 1
        else if !visited.exists(_._1 == currentPos._1.east) && !queue.exists(_._1 == currentPos._1.east) then
          queue.addOne(currentPos._1.east, currentPos._2)
        if map.getOrElse(currentPos._1.south, '-') != currentPos._2 then
          fencesS += 1
        else if !visited.exists(_._1 == currentPos._1.south) && !queue.exists(_._1 == currentPos._1.south) then
          queue.addOne(currentPos._1.south, currentPos._2)
        if map.getOrElse(currentPos._1.west, '-') != currentPos._2 then
          fencesW += 1
        else if !visited.exists(_._1 == currentPos._1.west) && !queue.exists(_._1 == currentPos._1.west) then
          queue.addOne(currentPos._1.west, currentPos._2)
        visited.update(currentPos._1, (region, (fencesN, fencesE, fencesS, fencesW)))
        regionPoss.addOne(currentPos._1, (fencesN, fencesE, fencesS, fencesW))
        unvisitedAll.remove(currentPos._1)
        queue.remove(0)
      cost += costOfRegion(regionPoss.toVector)
    cost

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
