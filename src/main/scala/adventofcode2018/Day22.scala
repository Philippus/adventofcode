package adventofcode2018

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day22:
  case class Pos(x: Int, y: Int)

  def riskLevel(depth: Long, target: Pos): Long =
    val erosionLevels   = mutable.Map.empty[Pos, Long]
    val geologicIndices = mutable.Map.empty[Pos, Long]

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
