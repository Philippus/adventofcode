package adventofcode2018

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2018.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  override val munitTimeout = Duration(1, "h")
  test("determines winning Elf's score for the samples"):
    assertEquals(highestScore(9, 25), 32L)
    assertEquals(highestScore(10, 1618), 8317L)
    assertEquals(highestScore(13, 7999), 146373L)
    assertEquals(highestScore(17, 1104), 2764L)
    assertEquals(highestScore(21, 6111), 54718L)
    assertEquals(highestScore(30, 5807), 37305L)

  test("determines winning Elf's score for the input"):
    val (players, lastMarble) = importLines()
    assertEquals(highestScore(players, lastMarble), 396136L)

  test("determines winning Elf's score with a 100 times more marbles for the input"):
    val (players, lastMarble) = importLines()
    assertEquals(highestScore(players, lastMarble * 100), 3183301184L)

  def importSampleLines(): (Int, Int) =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().next() match
        case s"$n players; last marble is worth $ps points" => (n.toInt, ps.toInt)
end Day09Suite
