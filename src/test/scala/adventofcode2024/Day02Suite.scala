package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day02.*
import munit.FunSuite

class Day02Suite extends FunSuite:
  test("counts safe reports for the sample"):
    assertEquals(countSafeReports(importSampleLines()), 2)

  test("counts safe reports for the input"):
    assertEquals(countSafeReports(importLines()), 402)

  test("counts safe reports with problem dampener for the sample"):
    assertEquals(countSafeReportsWithProblemDampener(importSampleLines()), 4)

  test("counts safe reports with problem dampener for the input"):
    assertEquals(countSafeReportsWithProblemDampener(importLines()), 455)

  def importSampleLines(): Seq[String] =
    Using.resource(Source.fromResource("2024/day02sampleinput.txt")): source =>
      source.getLines().toSeq
end Day02Suite
