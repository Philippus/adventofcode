package adventofcode2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day12:
  def grow(pots: String, notes: Map[String, Char], generations: Long): Long =
    @tailrec
    def loop(pots: String, generation: Int): Long =
      if generation == generations then
        pots.zipWithIndex.map:
          case (p, i) => if p == '#' then i - generations * 3 else 0
        .sum
      else
        val newPots = ("....." ++ pots ++ ".....").sliding(5).map: s =>
          notes.getOrElse(s, '.')
        .mkString
        loop(newPots, generation + 1)

    loop(pots, 0)

  def potsAfter50000000000Generations(pots: String, notes: Map[String, Char]): Long =
    val after2500 = grow(pots, notes, 2500)
    val after2501 = grow(pots, notes, 2501)
    val diff      = after2501 - after2500
    (50000000000L - 2500) * diff + after2500

  def handleLines(lines: Vector[String]): (String, Map[String, Char]) =
    val pots  = lines.head.substring("initial state: ".length)
    val notes = lines.drop(2).map:
      case s"$rule => $pot" => rule -> pot.head
    .toMap
    (pots, notes)

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day12
