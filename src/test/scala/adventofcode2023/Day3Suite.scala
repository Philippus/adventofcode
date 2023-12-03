package adventofcode2023

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

import adventofcode2023.day3.Day3.*
import munit.FunSuite

class Day3Suite extends FunSuite:
  test("finds symbols for a line"):
    assertEquals(findSymbols("617*......", 0), Seq(Pos(3, 0)))

  test("finds numbers for a line"):
    assertEquals(findNumbers("617*......", 0), Seq(Number("617", Seq(Pos(0, 0), Pos(1, 0), Pos(2, 0)))))

  test("finds symbols and adjacent numbers in a file"):
    val symbols = Using.resource(Source.fromResource("day3sampleinput.txt")): source =>
      val lines   = source.getLines
      var y       = 0
      var symbols = Seq.empty[Pos]
      while (lines.hasNext)
        symbols = symbols ++ findSymbols(lines.next, y)
        y += 1
      symbols

    val numbers = Using.resource(Source.fromResource("day3sampleinput.txt")): source =>
      val lines   = source.getLines
      var y       = 0
      var numbers = Seq.empty[Number]
      while (lines.hasNext)
        numbers = numbers ++ findNumbers(lines.next, y)
        y += 1
      numbers

    val filtered = filterAdjacent(numbers, symbols)

    assertEquals(filtered.map(_.value.toInt).sum, 4361)
end Day3Suite
