package adventofcode2025

import scala.io.Source
import scala.util.Using

import adventofcode2025.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("sums invalid ids for the sample"):
    val lines = importSampleLines()
    assertEquals(countInvalidIds(lines, isValid), 1227775554L)

  test("sums invalid ids for the input"):
    val lines = importLines()
    assertEquals(countInvalidIds(lines, isValid), 31210613313L)

  test("sums invalid ids using new rules for the sample"):
    val lines = importSampleLines()
    assertEquals(countInvalidIds(lines, isValidWithNewRules), 4174379265L)

  test("sums invalid ids using new rules for the input"):
    val lines = importLines()
    assertEquals(countInvalidIds(lines, isValidWithNewRules), 41823587546L)

  def importSampleLines(): List[(String, String)] =
    Using.resource(Source.fromResource("2025/Day02sampleinput.txt")): source =>
      source.getLines().toList.head.split(',').toList.map:
        case s"$a-$b" => (a, b)
end Day02Suite
