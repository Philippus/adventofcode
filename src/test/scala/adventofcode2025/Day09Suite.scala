package adventofcode2025

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2025.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  override val munitTimeout = Duration(3, "h")

  test("does inside/outside checks correctly"):
    val input     = importSampleLines()
    val positions = parse(input)
    val edges     = (positions.sliding(2).toList :+ List(positions.last, positions.head)).map(slide =>
      (slide.head, slide.last)
    )
    val grid      = parseAsGrid(input)
    println(drawGrid(grid))
    val grid2     = grid.clone()
    for
      y <- grid.indices
      x <- grid(y).indices
    do
      if (edges.map(edge => edgeCrossing(Pos(x, y), edge)).sum % 2 == 1) || edges.exists(edge =>
          onBorder(Pos(x, y), edge)
        )
      then
        grid2(y)(x) = 'i'
      else grid2(y)(x) = 'o'

    println(drawGrid(grid2))
    assert(true)

  test("finds largest rectangle for the sample"):
    val input     = importSampleLines()
    val positions = parse(input)
    assertEquals(largestSquare(positions), 50L)

  test("finds largest rectangle for the input"):
    val input     = importLines()
    val positions = parse(input)
    assertEquals(largestSquare(positions), 4781546175L)

  test("finds largest rectangle within the shape for the sample"):
    val input     = importSampleLines()
    val positions = parse(input)
    assertEquals(largestSquareInside(positions), 24L)

  test("finds largest rectangle within the shape for the input"):
    val input     = importLines()
    val positions = parse(input)
    assertEquals(largestSquareInside(positions), 1573359081L)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2025/day09sampleinput.txt")): source =>
      source.mkString
end Day09Suite
