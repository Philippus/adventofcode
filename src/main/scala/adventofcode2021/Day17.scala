package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day17:
  case class Area(xMin: Int, xMax: Int, yMin: Int, yMax: Int):
    def containsPos(pos: Pos): Boolean =
      pos.x >= xMin && pos.x <= xMax && pos.y >= yMin && pos.y <= yMax

    def beforePos(pos: Pos): Boolean =
      pos.x > xMax || pos.y < yMin

  case class Pos(x: Int, y: Int)

  case class Probe(pos: Pos, xVel: Int, yVel: Int)

  def withinTargetAreaAndHighestYpos(probe: Probe, area: Area): (Boolean, Int) =
    @tailrec
    def loop(probe: Probe, highestYpos: Int): (Boolean, Int) =
      if area.containsPos(probe.pos) then
        (true, highestYpos)
      else if area.beforePos(probe.pos) then
        (false, 0)
      else
        val newX    = probe.pos.x + probe.xVel
        val newY    = probe.pos.y + probe.yVel
        val newXvel = if probe.xVel > 0 then probe.xVel - 1 else if probe.xVel < 0 then probe.xVel + 1 else probe.xVel
        val newYvel = probe.yVel - 1
        loop(probe.copy(probe.pos.copy(newX, newY), newXvel, newYvel), math.max(highestYpos, newY))

    loop(probe, 0)

  def velocityValuesWithinTargetAreaWithHighestYpos(area: Area): Int =
    (for
      velX <- 0.to(area.xMax) // assumes xMin and xMax are positive
      velY <- -1000.to(1000)
      if withinTargetAreaAndHighestYpos(Probe(Pos(0, 0), velX, velY), area)._1
    yield withinTargetAreaAndHighestYpos(Probe(Pos(0, 0), velX, velY), area)._2).max

  def velocityValuesWithinTargetArea(area: Area): Int =
    (for
      velX <- 0.to(area.xMax) // assumes xMin and xMax are positive
      velY <- -1000.to(1000)
      if withinTargetAreaAndHighestYpos(Probe(Pos(0, 0), velX, velY), area)._1
    yield withinTargetAreaAndHighestYpos(Probe(Pos(0, 0), velX, velY), area)._2).length

  def parse(input: String): Area =
    input match
      case s"target area: x=$xMin..$xMax, y=$yMin..$yMax" =>
        Area(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt)

  def importLines(): String =
    Using.resource(Source.fromResource("2021/day17input.txt")): source =>
      source.mkString
end Day17
