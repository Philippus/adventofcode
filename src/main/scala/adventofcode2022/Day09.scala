package adventofcode2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09:
  case class Pos(x: Int, y: Int):
    def right: Pos = this.copy(x = this.x + 1)
    def left: Pos  = this.copy(x = this.x - 1)
    def down: Pos  = this.copy(y = this.y + 1)
    def up: Pos    = this.copy(y = this.y - 1)

    def isTouching(that: Pos): Boolean = math.abs(that.x - this.x) <= 1 && math.abs(that.y - this.y) <= 1

    def moveToward(that: Pos): Pos =
      if this.isTouching(that) then
        this
      else
        if this.x == that.x then // horizontal
          this.copy(y = (this.y + that.y) / 2)
        else if this.y == that.y then // vertical
          this.copy(x = (this.x + that.x) / 2)
        else if math.abs(that.y - this.y) == 1 then // diff like a knight's move
          this.copy(x = (this.x + that.x) / 2, y = that.y)
        else if math.abs(that.x - this.x) == 1 then // diff like a knight's move
          this.copy(x = that.x, y = (this.y + that.y) / 2)
        else                                        // diagonal (only in part 2 this occurs)
          this.copy(x = (this.x + that.x) / 2, y = (this.y + that.y) / 2)
  end Pos

  def createGrid(minX: Int, maxX: Int, minY: Int, maxY: Int): Seq[(Int, Int)] =
    for
      y <- minY to maxY
      x <- minX to maxX
    yield (x, y)

  def drawGrid(points: Set[Pos]): String =
    val grid = createGrid(points.minBy(_.x).x, points.maxBy(_.x).x, points.minBy(_.y).y, points.maxBy(_.y).y)
    grid.map:
      case (x, y) => (if x == points.minBy(_.x).x then "\n" else "") :+
          (if (x, y) == (0,0) then 's' else if points.exists(p => p.x == x && p.y == y) then '#' else '.')
    .mkString

  def simulateMotions(motions: List[String], knots: Int = 2, draw: Boolean = false): Int =
    @tailrec
    def loop(
        motions: List[String],
        hd: Pos,
        rope: Seq[Pos],
        acc: Set[Pos]
    ): Int =
      if motions.isEmpty then
        if draw then println(drawGrid(acc))
        acc.size
      else
        motions.head match
          case s"$dir 0"  =>
            loop(motions.tail, hd, rope, acc)
          case s"$dir $i" =>
            val newHd   = dir match
              case "R" => hd.right
              case "L" => hd.left
              case "D" => hd.down
              case "U" => hd.up
            val newRope = rope.scanLeft(newHd)((last, next) => next.moveToward(last)).tail
            loop(s"$dir ${i.toInt - 1}" :: motions.tail, newHd, newRope, acc + newRope.last)

    loop(motions, Pos(0, 0), Seq.fill(knots - 1)(Pos(0, 0)), Set(Pos(0, 0)))

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day09
