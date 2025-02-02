package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  test("determines total price of fencing all regions for the sample"):
    val map = handleLines(importSampleLines())
    assertEquals(totalPriceOfFencingAllRegions(map), 140L)

  test("determines total price of fencing all regions for the input"):
    val map = handleLines(importLines())
    assertEquals(totalPriceOfFencingAllRegions(map), 1461752L)

  test("determines discounted price of fencing all regions for the sample"):
    val map = handleLines(importSampleLines())
    assertEquals(discountedTotalPriceOfFencingAllRegions(map), 80L)

  test("determines discounted price of fencing all regions for the input"):
    val map = handleLines(importLines())
    assertEquals(discountedTotalPriceOfFencingAllRegions(map), 904114L)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day12Suite
