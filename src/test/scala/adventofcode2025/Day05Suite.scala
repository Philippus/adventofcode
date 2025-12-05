package adventofcode2025

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2025.Day05.*
import munit.FunSuite

class Day05Suite extends FunSuite:
  test("counts fresh ingredients for the sample"):
    val (ranges, ingredients) = parse(importSampleLines())
    assertEquals(countIngredientsInRanges(ranges, ingredients), 3L)

  test("counts fresh ingredients for the input"):
    val (ranges, ingredients) = parse(importLines())
    assertEquals(countIngredientsInRanges(ranges, ingredients), 726L)

  test("counts fresh ingredient ids for the sample"):
    val (ranges, ingredients) = parse(importSampleLines())
    assertEquals(countIngredientIds(ranges), 14L)

  test("counts fresh ingredient ids for the input"):
    val (ranges, ingredients) = parse(importLines())
    assertEquals(countIngredientIds(ranges), 354226555270043L)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2025/day05sampleinput.txt")): source =>
      source.mkString
end Day05Suite
