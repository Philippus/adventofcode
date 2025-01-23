package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day16.*
import munit.FunSuite

class Day16Suite extends FunSuite:
  test("calculates error scanning rate for the sample"):
    val (rules, yourTicket, nearbyTickets) = handleLines(importSampleLines())
    assertEquals(errorScanningRate(rules, nearbyTickets), 71)

  test("calculates error scanning rate for the input"):
    val (rules, yourTicket, nearbyTickets) = handleLines(importLines())
    assertEquals(errorScanningRate(rules, nearbyTickets), 19087)

  test("multiplies 'departure' fields for the input"):
    val (rules, yourTicket, nearbyTickets) = handleLines(importLines())
    assertEquals(multiplyDepartureFields(rules, yourTicket, nearbyTickets), 1382443095281L)

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toVector
end Day16Suite
