package adventofcode2018

import scala.io.Source
import scala.util.Using

import adventofcode2018.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("sums all metadata entries for the sample"):
    assertEquals(sumOfMetadataEntries(importSampleLines()), 138L)

  test("sums all metadata entries for the input"):
    assertEquals(sumOfMetadataEntries(importLines()), 42196L)

  test("finds root node value for the sample"):
    assertEquals(rootNodeValue(importSampleLines()), 66L)

  test("finds root node value for the input"):
    assertEquals(rootNodeValue(importLines()), 33649L)

  def importSampleLines(): Vector[Int] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().next().split(' ').map(_.toInt).toVector
end Day08Suite
