package adventofcode2021

import scala.io.Source
import scala.util.Using

import adventofcode2021.Day22.*
import munit.FunSuite

class Day22Suite extends FunSuite:
  test("counts turned on cubes inside the initialization procedure area for the sample"):
    val rebootSteps = parse(importSampleLines())
    assertEquals(executeRebootStepsInSmallRegion(rebootSteps), 39)

  test("counts turned on cubes inside the initialization procedure area for the input"):
    val rebootSteps = parse(importLines())
    assertEquals(executeRebootStepsInSmallRegion(rebootSteps), 587097)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2021/day22sampleinput.txt")): source =>
      source.mkString
end Day22Suite
