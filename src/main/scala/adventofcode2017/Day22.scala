package adventofcode2017

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

import adventofcode2017.Day22.Direction.*
import adventofcode2017.Day22.State.*

object Day22:
  case class Pos(x: Int, y: Int):
    def up: Pos    = Pos(x, y - 1)
    def right: Pos = Pos(x + 1, y)
    def down: Pos  = Pos(x, y + 1)
    def left: Pos  = Pos(x - 1, y)

    def move(direction: Direction): Pos =
      direction match
        case Up    => this.up
        case Right => this.right
        case Down  => this.down
        case Left  => this.left
  end Pos

  enum Direction:
    case Up, Down, Left, Right

    def turnRight: Direction = this match
      case Up    => Right
      case Right => Down
      case Down  => Left
      case Left  => Up

    def turnLeft: Direction = this match
      case Up    => Left
      case Right => Up
      case Down  => Right
      case Left  => Down

    def reverse: Direction = this match
      case Up    => Down
      case Right => Left
      case Down  => Up
      case Left  => Right
  end Direction

  enum State:
    case Clean, Weakened, Infected, Flagged
  end State

  def findStartPos(map: Map[Pos, Char]): Pos =
    Pos(map.maxBy(_._1.x)._1.x / 2, map.maxBy(_._1.y)._1.y / 2)

  def infectNodes(initialMap: Map[Pos, Char], maxBursts: Int, evolved: Boolean = false): Int =
    val map = initialMap.filter(_._2.==('#')).map:
      case (p, c) => (p, Infected)
    .to(mutable.Map)

    @tailrec
    def loop(currPos: Pos, currDir: Direction, burst: Int, causedInfection: Int): Int =
      if burst == maxBursts then
        causedInfection
      else
        map.getOrElse(currPos, Clean) match
          case Clean    =>
            if evolved then
              map.update(currPos, Weakened)
              loop(currPos.move(currDir.turnLeft), currDir.turnLeft, burst + 1, causedInfection)
            else
              map.update(currPos, Infected)
              loop(currPos.move(currDir.turnLeft), currDir.turnLeft, burst + 1, causedInfection + 1)
          case Weakened =>
            map.update(currPos, Infected)
            loop(currPos.move(currDir), currDir, burst + 1, causedInfection + 1)
          case Infected =>
            if evolved then
              map.update(currPos, Flagged)
              loop(currPos.move(currDir.turnRight), currDir.turnRight, burst + 1, causedInfection)
            else
              map.remove(currPos)
              loop(currPos.move(currDir.turnRight), currDir.turnRight, burst + 1, causedInfection)
          case Flagged  =>
            map.remove(currPos)
            loop(currPos.move(currDir.reverse), currDir.reverse, burst + 1, causedInfection)

    loop(findStartPos(initialMap), Up, 0, 0)

  def parse(line: String, y: Int): Map[Pos, Char] =
    line.zipWithIndex.collect:
      case (char, x) => Pos(x, y) -> char
    .toMap

  def handleLines(lines: List[String]): Map[Pos, Char] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toMap

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day22
