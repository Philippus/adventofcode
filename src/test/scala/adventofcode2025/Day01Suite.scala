package adventofcode2025

import scala.io.Source
import scala.util.Using

import adventofcode2025.Day01.*
import munit.FunSuite

class Day01Suite extends FunSuite:
  test("calculates dial pointing to zero for the sample"):
    assertEquals(dialAtZero(importSampleLines()), 3)

  test("calculates dial pointing to zero for the input"):
      assertEquals(dialAtZero(importLines()), 1105)

  test("calculates any click pointing to zero for the sample"):
    assertEquals(clicks(importSampleLines()), 6)

  test("calculates any click pointing to zero for the input"):
    assertEquals(clicks(importLines()), 6599)

  def importSampleLines(): Seq[Int] =
    Using.resource(Source.fromResource("2025/day01sampleinput.txt")): source =>
      source.getLines().toSeq.map:
        case s"L$n" => n.toInt * - 1
        case s"R$n" => n.toInt
end Day01Suite
