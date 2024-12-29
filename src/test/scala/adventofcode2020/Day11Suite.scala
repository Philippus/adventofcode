package adventofcode2020

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2020.Day11.*
import munit.FunSuite

class Day11Suite extends FunSuite:
  override val munitTimeout = Duration(2, "d")

  test("draws the seats for the sample"):
    val seats = handleLines(importSampleLines())
    println(drawGrid(seats, 10, 10))

  test("seats the passengers for the sample"):
    val seats = handleLines(importSampleLines())
    assertEquals(seatPassengers(seats), 37)

  test("seats the passengers for the input"):
    val seats = handleLines(importLines())
    assertEquals(seatPassengers(seats), 2126)

  test("seats the passengers with the visibility method for the sample"):
    val seats = handleLines(importSampleLines())
    assertEquals(seatPassengersWithVisibility(seats), 26)

  test("seats the passengers with the visibility method for the input"):
    val seats = handleLines(importLines())
    assertEquals(seatPassengersWithVisibility(seats), 1914)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day11Suite
