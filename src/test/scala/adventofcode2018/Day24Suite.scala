package adventofcode2018

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2018.Day24.*
import munit.FunSuite

class Day24Suite extends FunSuite:
  override val munitTimeout = Duration(200, "s")
  test("remaining units of winning army for the sample"):
    val (army1, army2) = handleLines(importSampleLines())
    assertEquals(battle(army1, army2), 5216)

  test("remaining units of winning army for the input"):
    val (army1, army2) = handleLines(importLines())
    assertEquals(battle(army1, army2), 16006)

  test("find minimal boost for the sample"):
    val (army1, army2) = handleLines(importSampleLines())
    assertEquals(battleWithBoost(army1, army2), 51)

  test("find minimal boost for the input"):
    val (army1, army2) = handleLines(importLines())
    assertEquals(battleWithBoost(army1, army2), 6221)

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toVector
end Day24Suite
