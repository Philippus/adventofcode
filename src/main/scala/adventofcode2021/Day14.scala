package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day14:
  def insertPairs(polymer: String, rules: Map[String, String], steps: Int): String =
    @tailrec
    def loop(polymer: String, step: Int): String =
      if step == steps then
        polymer
      else
        val newPolymer = polymer.sliding(2).map(pair => pair.head + rules(pair)).mkString + polymer.last
        loop(newPolymer, step + 1)
    loop(polymer, 0)

  def mostMinusLeastCommon(polymer: String): Int =
    val groupedById = polymer.groupBy(identity).view.mapValues(_.length)
    groupedById.maxBy(_._2)._2 - groupedById.minBy(_._2)._2

  def handleLines(lines: List[String]): (String, Map[String, String]) =
    val polymer = lines.head
    val rules   = lines.drop(2).map:
      case s"$a -> $b" => a -> b
    .toMap
    (polymer, rules)

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2021/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day14
