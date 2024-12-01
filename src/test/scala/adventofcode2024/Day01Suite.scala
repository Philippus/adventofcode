package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day01.*
import munit.FunSuite

class Day01Suite extends FunSuite:
  test("calculates total distance between lists for the sample"):
    assertEquals(distanceBetweenLists(importSampleLines()), 11)

  test("calculates total distance between lists"):
    assertEquals(distanceBetweenLists(importLines()), 2367773)

  test("calculates similarity score between lists for the sample"):
    assertEquals(similarityBetweenLists(importSampleLines()), 31)

  test("calculates similarity score between lists"):
    assertEquals(similarityBetweenLists(importLines()), 21271939)

  def importSampleLines(): Seq[String] =
    Using.resource(Source.fromResource("2024/day01sampleinput.txt")): source =>
      source.getLines().toSeq
end Day01Suite
