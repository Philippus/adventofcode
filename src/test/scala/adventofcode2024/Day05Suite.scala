package adventofcode2024

import scala.io.Source
import scala.util.{Try, Using}

import adventofcode2024.Day05.*
import munit.FunSuite

class Day05Suite extends FunSuite:
  test("adds up middle page numbers of correctly ordered updates for the sample input"):
    assertEquals(addUpMiddlePageNumbersForValidUpdates(importSampleLines()), 143)

  test("adds up middle page numbers of correctly ordered updates for the input"):
    assertEquals(addUpMiddlePageNumbersForValidUpdates(importLines()), 5166)

  test("adds up middle page numbers of correctly ordered updates for the sample input"):
    assertEquals(addUpMiddlePageNumbersOfInvalidUpdates(importSampleLines()), 123)

  test("adds up middle page numbers of correctly ordered updates for the input"):
    assertEquals(addUpMiddlePageNumbersOfInvalidUpdates(importLines()), 4679)

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq
end Day05Suite
