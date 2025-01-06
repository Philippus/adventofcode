package adventofcode2021

import scala.io.Source
import scala.util.Using

import adventofcode2021.Day14.*
import munit.FunSuite

class Day14Suite extends FunSuite:
  test("inserts pairs in the polymers for the sample"):
    val lines            = importSampleLines()
    val (polymer, rules) = handleLines(lines)
    assertEquals(insertPairs(polymer, rules, 1), "NCNBCHB")
    assertEquals(insertPairs(polymer, rules, 2), "NBCCNBBBCBHCB")
    assertEquals(insertPairs(polymer, rules, 3), "NBBBCNCCNBBNBNBBCHBHHBCHB")
    assertEquals(insertPairs(polymer, rules, 4), "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")

  test("calculates most minus least common element of polymer for the sample"):
    val lines            = importSampleLines()
    val (polymer, rules) = handleLines(lines)
    val newPolymer       = insertPairs(polymer, rules, 10)
    assertEquals(mostMinusLeastCommon(newPolymer), 1588)

  test("calculates most minus least common element of polymer for the input"):
    val lines            = importLines()
    val (polymer, rules) = handleLines(lines)
    val newPolymer       = insertPairs(polymer, rules, 10)
    assertEquals(mostMinusLeastCommon(newPolymer), 2010)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2021/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day14Suite
