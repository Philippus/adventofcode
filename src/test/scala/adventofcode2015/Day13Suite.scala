package adventofcode2015

import scala.io.Source
import scala.util.Using

import adventofcode2015.Day13.*
import munit.FunSuite

class Day13Suite extends FunSuite:
  test("handle a line"):
    assertEquals(handleLine("Alice would gain 54 happiness units by sitting next to Bob.")._2, 54)
    assertEquals(handleLine("Bob would lose 63 happiness units by sitting next to David.")._2, -63)

  test("calculates the happiness for a seating arrangement with the values in the sample input"):
    Using.resource(Source.fromResource("2015/day13sampleinput.txt")): source =>
      val values = source.getLines().map(handleLine).toSeq
      assertEquals(calculateHappiness(List("Alice", "Bob", "Carol", "David"), values), 330)

  test("calculates the maximum happiness for the seating in the input"):
    assertEquals(maximumHappinessForInputFile(None), 618)

  test("calculates the maximum happiness for the seating in the input when I am seated as well"):
    assertEquals(maximumHappinessForInputFile(Some("Philippus")), 601)

end Day13Suite
