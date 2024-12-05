package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day05:
  @tailrec
  def isCorrectlyOrdered(update: Seq[String], orderingRules: Seq[String]): Boolean =
    if update.length <= 1 then
      true
    else
      orderingRules.forall:
        case s"$a|$b" if b == update.head && update.tail.contains(a) => false
        case _                                                       => true
      && isCorrectlyOrdered(update.tail, orderingRules)

  def correctlyOrder(wrongUpdate: Seq[String], orderingRules: Seq[String]): Seq[String] =
    wrongUpdate.sortWith((a, b) =>
      orderingRules.map:
        case s"$x|$y" if x == a && y == b => true
        case s"$x|$y" if x == b && y == a => false
        case _                            => false
      .find(identity).getOrElse(false)
    )

  def addUpMiddlePageNumbers(updates: Seq[String]): Int =
    updates.filter(_.nonEmpty).map: valid =>
      val split = valid.split(",")
      split.slice(split.length / 2, split.length / 2 + 1).head
    .map(_.toInt).sum

  def separateSections(lines: Seq[String]): (Seq[String], Seq[String]) =
    (lines.takeWhile(_.nonEmpty), lines.dropWhile(_.nonEmpty).filter(_.nonEmpty))

  def addUpMiddlePageNumbersForValidUpdates(lines: Seq[String]): Int =
    val (rules, updates) = separateSections(lines)
    val valids           = updates.filter(update =>
      isCorrectlyOrdered(update.split(","), rules)
    )
    addUpMiddlePageNumbers(valids)

  def addUpMiddlePageNumbersOfInvalidUpdates(lines: Seq[String]): Int =
    val (rules, updates)  = separateSections(lines)
    val correctedInvalids = updates.filter(update =>
      !isCorrectlyOrdered(update.split(","), rules)
    ).map(invalid => correctlyOrder(invalid.split(','), rules).mkString(","))
    addUpMiddlePageNumbers(correctedInvalids)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day05
