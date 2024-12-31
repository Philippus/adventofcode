package adventofcode2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day13:
  @tailrec
  def checkLines(lines: Vector[String], i: Int, j: Int): Boolean =
    if i >= 0 && j < lines.length then
      lines(i) == lines(j) && checkLines(lines, i - 1, j + 1)
    else
      true

  def findReflections(patterns: Seq[String]): Int =
    val horizontal = patterns.flatMap: pattern =>
      val lines = pattern.split("\n").toVector
      lines.zipWithIndex.find:
        case (line, i) => i < lines.length - 1 && checkLines(lines, i, i + 1)
      .map(l => l._2 + 1)
    .sum
    val vertical   = patterns.flatMap: pattern =>
      val lines = pattern.split("\n").toVector.transpose.map(_.mkString)
      lines.zipWithIndex.find:
        case (line, i) => i < lines.length - 1 && checkLines(lines, i, i + 1)
      .map(l => l._2 + 1)
    .sum

    horizontal * 100 + vertical

  def patternAlternatives(pattern: String): Seq[String] =
    for
      i <- pattern.indices
      if pattern(i) != '\n'
    yield pattern.updated(i, if pattern(i) == '.' then '#' else '.')

  def findReflectionsDespiteSmudges(patterns: Seq[String]): Int =
    val horizontal = patterns.flatMap: pattern =>
      val lines      = pattern.split("\n").toVector
      val oldPattern = lines.zipWithIndex.find:
        case (line, i) => i < lines.length - 1 && checkLines(lines, i, i + 1)
      .map(_._2)

      val patterns = patternAlternatives(pattern)
      patterns.flatMap: pattern =>
        val lines = pattern.split("\n").toVector
        lines.zipWithIndex.find:
          case (line, i) => i < lines.length - 1 && checkLines(lines, i, i + 1) && !oldPattern.contains(i)
        .map(_._2 + 1)
      .distinct
    .sum
    val vertical   = patterns.flatMap: pattern =>
      val lines      = pattern.split("\n").toVector.transpose.map(_.mkString)
      val oldPattern = lines.zipWithIndex.find:
        case (line, i) => i < lines.length - 1 && checkLines(lines, i, i + 1)
      .map(_._2)

      val patterns = patternAlternatives(pattern)
      patterns.flatMap: pattern =>
        val lines = pattern.split("\n").toVector.transpose.map(_.mkString)
        lines.zipWithIndex.collect:
          case (line, i) if i < lines.length - 1 && checkLines(lines, i, i + 1) && !oldPattern.contains(i) => i + 1
      .distinct
    .sum

    horizontal * 100 + vertical
  def importLines(): Vector[String]                             =
    Using.resource(Source.fromResource(s"2023/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().mkString("\n").split("\n\n").toVector
end Day13
