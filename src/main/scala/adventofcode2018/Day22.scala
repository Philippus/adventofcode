package adventofcode2018

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

import adventofcode2018.Day22.RegionType.{Narrow, Rocky, Wet}
import adventofcode2018.Day22.Tool.{ClimbingGear, Neither, Torch}

object Day22:
  case class Pos(x: Int, y: Int):
    def north: Pos            = Pos(x, y - 1)
    def east: Pos             = Pos(x + 1, y)
    def south: Pos            = Pos(x, y + 1)
    def west: Pos             = Pos(x - 1, y)
    def withinBounds: Boolean = x >= 0 && y >= 0
  end Pos

  enum RegionType:
    case Rocky, Wet, Narrow

    def allowsTool(tool: Tool): Boolean = this match
      case Rocky  => tool == ClimbingGear || tool == Torch
      case Wet    => tool == ClimbingGear || tool == Neither
      case Narrow => tool == Torch || tool == Neither
  end RegionType

  enum Tool:
    case Torch, ClimbingGear, Neither

  def riskLevel(depth: Long, target: Pos): Long =
    val erosionLevels   = mutable.Map.empty[Pos, Long]
    val geologicIndices = mutable.Map.empty[Pos, Long]
    val regionTypes     = mutable.Map.empty[Pos, RegionType]

    def regionType(pos: Pos, depth: Long, target: Pos): RegionType =
      regionTypes.getOrElseUpdate(
        pos,
        erosionLevel(pos, depth, target) % 3 match
          case 0 => Rocky
          case 1 => Wet
          case 2 => Narrow
      )

    def erosionLevel(pos: Pos, depth: Long, target: Pos): Long =
      erosionLevels.getOrElse(pos, (geologicIdx(pos, depth, target) + depth) % 20183)

    def geologicIdx(pos: Pos, depth: Long, target: Pos): Long = geologicIndices.getOrElseUpdate(
      pos,
      (pos.x, pos.y) match
        case (0, 0)               => 0L
        case (target.x, target.y) => 0L
        case (x, 0)               => x * 16807L
        case (0, y)               => y * 48271L
        case (x, y)               => erosionLevel(Pos(x - 1, y), depth, target) * erosionLevel(Pos(x, y - 1), depth, target)
    )

    (for
      x <- 0 to target.x
      y <- 0 to target.y
    yield erosionLevel(Pos(x, y), depth, target) % 3)
      .sum

  def fewestMinutesToTarget(depth: Long, target: Pos): Long =
    val erosionLevels   = mutable.Map.empty[Pos, Long]
    val geologicIndices = mutable.Map.empty[Pos, Long]
    val regionTypes     = mutable.Map.empty[Pos, RegionType]

    def regionType(pos: Pos, depth: Long, target: Pos): RegionType =
      regionTypes.getOrElseUpdate(
        pos,
        erosionLevel(pos, depth, target) % 3 match
          case 0 => Rocky
          case 1 => Wet
          case 2 => Narrow
      )

    def erosionLevel(pos: Pos, depth: Long, target: Pos): Long =
      erosionLevels.getOrElse(pos, (geologicIdx(pos, depth, target) + depth) % 20183)

    def geologicIdx(pos: Pos, depth: Long, target: Pos): Long = geologicIndices.getOrElseUpdate(
      pos,
      (pos.x, pos.y) match
        case (0, 0)               => 0L
        case (target.x, target.y) => 0L
        case (x, 0)               => x * 16807L
        case (0, y)               => y * 48271L
        case (x, y)               => erosionLevel(Pos(x - 1, y), depth, target) * erosionLevel(Pos(x, y - 1), depth, target)
    )

    val unvisited: mutable.Map[(Pos, Tool), Long] = Map((Pos(0, 0), Torch) -> 0L).to(mutable.Map)

    val visited = mutable.Map[(Pos, Tool), Long]()

    while !visited.contains((target, Torch)) do
      val ((currentNode, currentTool), currentVal) = unvisited.minBy(_._2)
      if currentNode.north.withinBounds && !visited.contains(currentNode.north, currentTool) && regionType(
          currentNode.north,
          depth,
          target
        ).allowsTool(currentTool)
      then
        unvisited.update(
          (currentNode.north, currentTool),
          math.min(currentVal + 1, unvisited.getOrElse((currentNode.north, currentTool), Long.MaxValue))
        )
      if currentNode.east.withinBounds && !visited.contains(currentNode.east, currentTool) && regionType(
          currentNode.east,
          depth,
          target
        ).allowsTool(currentTool)
      then
        unvisited.update(
          (currentNode.east, currentTool),
          math.min(currentVal + 1, unvisited.getOrElse((currentNode.east, currentTool), Long.MaxValue))
        )
      if currentNode.south.withinBounds && !visited.contains(currentNode.south, currentTool) && regionType(
          currentNode.south,
          depth,
          target
        ).allowsTool(currentTool)
      then
        unvisited.update(
          (currentNode.south, currentTool),
          math.min(currentVal + 1, unvisited.getOrElse((currentNode.south, currentTool), Long.MaxValue))
        )
      if currentNode.west.withinBounds && !visited.contains(currentNode.west, currentTool) && regionType(
          currentNode.west,
          depth,
          target
        ).allowsTool(currentTool)
      then
        unvisited.update(
          (currentNode.west, currentTool),
          math.min(currentVal + 1, unvisited.getOrElse((currentNode.west, currentTool), Long.MaxValue))
        )
      (regionType(currentNode, depth, target), currentTool) match
        case (Rocky, Torch) if !visited.contains(currentNode, ClimbingGear) =>
          unvisited.update(
            (currentNode, ClimbingGear),
            math.min(currentVal + 7, unvisited.getOrElse((currentNode, ClimbingGear), Long.MaxValue))
          )
        case (Rocky, ClimbingGear) if !visited.contains(currentNode, Torch) =>
          unvisited.update(
            (currentNode, Torch),
            math.min(currentVal + 7, unvisited.getOrElse((currentNode, Torch), Long.MaxValue))
          )
        case (Wet, ClimbingGear) if !visited.contains(currentNode, Neither) =>
          unvisited.update(
            (currentNode, Neither),
            math.min(currentVal + 7, unvisited.getOrElse((currentNode, Neither), Long.MaxValue))
          )
        case (Wet, Neither) if !visited.contains(currentNode, ClimbingGear) =>
          unvisited.update(
            (currentNode, ClimbingGear),
            math.min(currentVal + 7, unvisited.getOrElse((currentNode, ClimbingGear), Long.MaxValue))
          )
        case (Narrow, Torch) if !visited.contains(currentNode, Neither)     =>
          unvisited.update(
            (currentNode, Neither),
            math.min(currentVal + 7, unvisited.getOrElse((currentNode, Neither), Long.MaxValue))
          )
        case (Narrow, Neither) if !visited.contains(currentNode, Torch)     =>
          unvisited.update(
            (currentNode, Torch),
            math.min(currentVal + 7, unvisited.getOrElse((currentNode, Torch), Long.MaxValue))
          )
        case _                                                              => ()
      unvisited.remove((currentNode, currentTool))
      visited.update((currentNode, currentTool), currentVal)
    visited((target, Torch))

  def importLines(): (Long, Pos) =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        val lines  = source.getLines().toVector
        val depth  = lines.head match
          case s"depth: $d" =>
            d.toLong
        val target = lines.last match
          case s"target: $x,$y" =>
            Pos(x.toInt, y.toInt)
        (depth, target)
end Day22
