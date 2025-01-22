package adventofcode2017

import scala.io.Source
import scala.util.Using

import adventofcode2017.Day24.*
import munit.FunSuite

class Day24Suite extends FunSuite:
  test("finds strongest bridge for the sample"):
    val components = handleLines(importSampleLines())
    assertEquals(bridgeOfMaximumStrength(components), 31)

  test("finds strongest bridge for the input"):
    val components = handleLines(importLines())
    assertEquals(bridgeOfMaximumStrength(components), 1906)

  test("finds longest bridge of maximum strength for the sample"):
    val components = handleLines(importSampleLines())
    assertEquals(longestBridgeOfMaximumStrength(components), 19)

  test("finds longest bridge of maximum strength for the input"):
    val components = handleLines(importLines())
    assertEquals(longestBridgeOfMaximumStrength(components), 1824)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day24Suite
