package adventofcode2019

import scala.io.Source
import scala.util.Using

import adventofcode2019.Day03.*
import munit.FunSuite

class Day03Suite extends FunSuite:
  test("finds Manhattan distance of closest intersection for the sample"):
    val wires = handleLines(importSampleLines())
    assertEquals(manhattanDistanceOfClosestIntersection(wires.head, wires.last), 6)

  test("finds Manhattan distance of closest intersection for the input"):
    val wires = handleLines(importLines())
    assertEquals(manhattanDistanceOfClosestIntersection(wires.head, wires.last), 1195)

  def importSampleLines(): Seq[String] =
    Using.resource(Source.fromResource("2019/day03sampleinput.txt")): source =>
      source.getLines().toSeq
end Day03Suite
