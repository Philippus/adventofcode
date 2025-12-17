package adventofcode2021

import scala.io.Source
import scala.util.Using

import adventofcode2021.Day13.*
import munit.FunSuite

class Day13Suite extends FunSuite:
  test("counts dots after folding paper once for the sample"):
    val (positions, folds) = parse(importSampleLines())
    assertEquals(foldPaperOnceAndCountDots(positions, folds), 17)

  test("counts dots after folding paper once for the input"):
    val (positions, folds) = parse(importLines())
    assertEquals(foldPaperOnceAndCountDots(positions, folds), 687)

  test("draws the grid after folding paper for the sample"):
    val (positions, folds) = parse(importSampleLines())
    println(drawGrid(foldAll(positionsToGrid(positions, folds), folds))) // shows a square

  test("draws the grid after folding paper for the input"):
    val (positions, folds) = parse(importLines())
    println(drawGrid(foldAll(positionsToGrid(positions, folds), folds))) // shows FGKCKBZG

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2021/day13sampleinput.txt")): source =>
      source.mkString
end Day13Suite
