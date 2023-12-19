package adventofcode2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day01:
  case class Elf(foods: Seq[Int])

  @tailrec
  def loop(lines: Seq[String], acc: Seq[Elf] = Seq.empty): Seq[Elf] =
    if lines.indexWhere(_.isEmpty) > 0 then
      val (elfFoods, linesLeft) = lines.splitAt(lines.indexWhere(_.isEmpty))
      loop(linesLeft.tail, acc :+ Elf(elfFoods.filter(_.nonEmpty).map(_.toInt)))
    else
      acc

  def calculateElfWithMostCalories(): Int =
    val elves = importLines()
    elves.map(_.foods.sum).max

  def calculateSumOfTopThreeElves(): Int =
    val elves = importLines()
    elves.map(_.foods.sum).sorted.takeRight(3).sum

  def importLines(): Seq[Elf] =
    Using.resource(Source.fromResource("2022/day01input.txt")): source =>
      loop(source.getLines().toSeq)
end Day01
