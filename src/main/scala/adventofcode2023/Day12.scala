package adventofcode2023

import scala.io.Source
import scala.util.Using

object Day12:
  def calculateContinuousGroups(conditionRecord: String): String =
    conditionRecord.split('.').filter(_.nonEmpty).map(_.length).mkString(",")

  def generateCandidates(conditionRecord: String, acc: String = ""): Seq[String] =
    if conditionRecord.isEmpty then
      Seq(acc)
    else
      conditionRecord.head match
        case '?'   =>
          generateCandidates(conditionRecord.tail, acc :+ '#') ++ generateCandidates(conditionRecord.tail, acc :+ '.')
        case other =>
          generateCandidates(conditionRecord.tail, acc :+ other)

  def calculatesArrangements(records: Seq[String]): Long =
    records.map:
      case s"$conditionRecord $continuousGroup" =>
        generateCandidates(conditionRecord).map(calculateContinuousGroups).count(group => group == continuousGroup)
    .sum

  def unfold(record: String): String =
    record match
      case s"$conditionRecord $continuousGroup" =>
        Seq.fill(5)(conditionRecord).mkString("?") ++ " " ++ Seq.fill(5)(continuousGroup).mkString(",")

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2023/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day12
