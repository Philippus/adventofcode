package adventofcode2024

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day19:
  def countPossible(patterns: Seq[String], designs: Seq[String]): Int =
    def loop(design: String): Boolean =
      if design.isEmpty then
        true
      else
        patterns.exists: pattern =>
          if design.startsWith(pattern) then
            loop(design.drop(pattern.length))
          else
            false
    designs.map(design => loop(design)).count(identity)

  def countPossibleWays(patterns: Seq[String], designs: Seq[String]): Long =
    val map: mutable.Map[String, Long] = mutable.Map[String, Long]()
    def loop(design: String): Long     =
      if design.isEmpty then
        1L
      else
        patterns.map: pattern =>
          if design.startsWith(pattern) then
            map.getOrElseUpdate(design.drop(pattern.length), loop(design.drop(pattern.length)))
          else
            0L
        .sum

    designs.map(design => loop(design)).sum

  def handleLines(lines: Seq[String]): (Seq[String], Seq[String]) =
    (lines.head.split(", ").toSeq, lines.drop(2))

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day19
