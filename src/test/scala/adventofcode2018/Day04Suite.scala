package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day04.*
import munit.FunSuite

class Day04Suite extends FunSuite:
  test("finds guard that has the most minutes asleep for the sample"):
    assertEquals(applyStrategy(importSampleLines()), 240)

  test("finds guard that has the most minutes asleep for the input"):
    assertEquals(applyStrategy(importLines()), 11367)

  test("finds guard that is most frequently asleep on the same minute for the sample"):
    assertEquals(applyStrategy(importSampleLines(), 2), 4455)

  test("finds guard that is most frequently asleep on the same minute for the input"):
    assertEquals(applyStrategy(importLines(), 2), 36896)

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq.sorted
end Day04Suite
