package adventofcode2022

import scala.io.Source
import scala.util.Using

import adventofcode2022.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  test("draws visited positions"):
    val motions = importSampleLines()
    simulateMotions(motions, 2, true)
    val otherMotions = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20".split('\n').toList
    simulateMotions(otherMotions, 10, true)

  test("determines visited positions of the tail with 2 knots for the sample"):
    val motions = importSampleLines()
    assertEquals(simulateMotions(motions, 2), 13)

  test("determines visited positions of the tail with 2 knots for the input"):
    val motions = importLines()
    assertEquals(simulateMotions(motions, 2), 6067)

  test("determines visited positions of the tail with 10 knots for the sample"):
    val motions = importSampleLines()
    assertEquals(simulateMotions(motions, 10), 1)

  test("determines visited positions of the tail with 10 knots for the input"):
    val motions = importLines()
    assertEquals(simulateMotions(motions, 10), 2471)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day09Suite
