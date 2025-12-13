package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day22.*
import munit.FunSuite

class Day22Suite extends FunSuite:
  test("calculates winning player's score for the sample"):
    val input = importSampleLines()
    val decks = parse(input)
    assertEquals(play(decks.head, decks.last), 306L)

  test("calculates winning player's score for the input"):
    val input = importLines()
    val decks = parse(input)
    assertEquals(play(decks.head, decks.last), 35370L)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2020/day22sampleinput.txt")): source =>
      source.mkString
end Day22Suite
