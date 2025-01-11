package adventofcode2017

import scala.io.Source
import scala.util.Using

object Day11:
  case class Pos(x: Int, y: Int):
    def n: Pos  = Pos(x, y - 1)
    def ne: Pos = Pos(x - 1, y)
    def se: Pos = Pos(x - 1, y + 1)
    def s: Pos  = Pos(x, y + 1)
    def sw: Pos = Pos(x + 1, y)
    def nw: Pos = Pos(x + 1, y - 1)

    def move(dir: String): Pos =
      dir match
        case "n"  => this.n
        case "ne" => this.ne
        case "se" => this.se
        case "s"  => this.s
        case "sw" => this.sw
        case "nw" => this.nw
  end Pos

  def findTargetPos(moves: Seq[String]): Pos =
    moves.foldLeft(Pos(0, 0))((a, b) => a.move(b))

  def stepsToTarget(moves: Seq[String]): Int =
    val targetPos = findTargetPos(moves)
    targetPos match
      case Pos(x, y) if (x >= 0 && y >= 0) || (x <= 0 && y <= 0) => math.abs(x) + math.abs(y)          // Q1 or Q3
      case Pos(x, y)                                             => math.max(math.abs(x), math.abs(y)) // Q2 or Q4

  def stepsToFurthestPosition(moves: Seq[String]): Int =
    val (q1orQ3, q2orQ4) =
      moves.scanLeft(Pos(0, 0))((a, b) => a.move(b)).partition(p => (p.x >= 0 && p.y >= 0) || p.x <= 0 && p.y <= 0)
    (q1orQ3.map(p => math.abs(p.x) + math.abs(p.y)) ++
      q2orQ4.map(p => math.max(math.abs(p.x), math.abs(p.y)))).max

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next().split(',')
end Day11
