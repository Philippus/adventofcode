package adventofcode2025

import scala.io.Source
import scala.util.Using

import nl.gn0s1s.between.*

object Day05:
  def countIngredientsInRanges(ranges: List[(Long, Long)], ingredients: List[Long]): Long =
    ingredients.count: ingredient =>
      ranges.exists: range =>
        range._1 <= ingredient && range._2 >= ingredient

  def countIngredientIds(ranges: List[(Long, Long)]): Long =
    val intervals = ranges.map: range =>
      Interval[Long](range._1, range._2)
    .collect:
      case i if i.nonEmpty => i.get
    .sortBy(_.`-`)

    val rangesOfOne = ranges.collect:
      case range if range._1 == range._2 =>
        range._1

    val mergedIntervals = intervals.foldLeft(List.empty[Interval[Long]]):
      case (List(), j) => List(j)
      case (s, j)      => s.last.findRelation(j) match
          case `<` | `>` => s :+ j
          case r         => s.init :+ s.last.span(j)

    val sumofIngredientIdsInIntervals = mergedIntervals.map(i => i.`+` - i.`-` + 1).sum

    val sumOfIngredientIdsInRangesOfOne =
      countIngredientsInRanges(mergedIntervals.map(i => (i.`-`, i.`+`)), rangesOfOne)

    sumofIngredientIdsInIntervals + sumOfIngredientIdsInRangesOfOne

  def parse(input: String): (List[(Long, Long)], List[Long]) =
    val split       = input.split("\n\n")
    val ranges      = split.head.split("\n").map:
      case s"$from-$to" => (from.toLong, to.toLong)
    val ingredients = split.last.split("\n").map(_.toLong)
    (ranges.toList, ingredients.toList)

  def importLines(): String =
    Using.resource(Source.fromResource("2025/day05input.txt")): source =>
      source.mkString
end Day05
