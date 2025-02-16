package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day25:
  case class FixedPoint(a: Int, b: Int, c: Int, d: Int):
    def dist(other: FixedPoint): Int =
      math.abs(a - other.a) + math.abs(b - other.b) + math.abs(c - other.c) + math.abs(d - other.d)
  end FixedPoint

  def constellations(fixedPoints: Vector[FixedPoint]): Int =
    @tailrec
    def loop(fixedPoints: Vector[FixedPoint], constellations: Vector[Vector[FixedPoint]]): Int =
      if fixedPoints.isEmpty then
        constellations.length
      else
        val hd           = fixedPoints.head
        val (close, far) = constellations.partition: c =>
          c.map(_.dist(hd)).exists(_.<=(3))
        loop(fixedPoints.tail, far :+ (close.flatten :+ hd))

    loop(fixedPoints, Vector.empty[Vector[FixedPoint]])

  def handleLines(lines: Vector[String]): Vector[FixedPoint] =
    lines.map:
      case s"$a,$b,$c,$d" => FixedPoint(a.toInt, b.toInt, c.toInt, d.toInt)

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day25
