package adventofcode2020

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2020.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("moves the ship for the sample"):
    val instructions = importSampleLines()
    assertEquals(moveShip(instructions), 25)

  test("moves the ship for the input"):
    val instructions = importLines()
    assertEquals(moveShip(instructions), 1152)

  test("moves the waypoint and the ship for the sample"):
    val instructions = importSampleLines()
    assertEquals(moveWaypointAndShip(instructions), 286)

  test("moves the waypoint and the ship for the input"):
    val instructions = importLines()
    assertEquals(moveWaypointAndShip(instructions), 58637)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day12Suite
