package adventofcode2025

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2025.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("connects junction boxes for the sample"):
    val input = importSampleLines()
    val boxes = parse(input)
    assertEquals(connectBoxes(boxes, 10), 40)

  test("connects junction boxes for the input"):
    val input = importLines()
    val boxes = parse(input)
    assertEquals(connectBoxes(boxes, 1000), 112230)

  test("connects junction boxes into one circuit for the sample"):
    val input = importSampleLines()
    val boxes = parse(input)
    assertEquals(connectBoxesIntoOneCircuit(boxes), 25272L)

  test("connects junction boxes into one circuit for the input"):
    val input = importLines()
    val boxes = parse(input)
    assertEquals(connectBoxesIntoOneCircuit(boxes), 2573952864L)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2025/day08sampleinput.txt")): source =>
      source.mkString
end Day08Suite
