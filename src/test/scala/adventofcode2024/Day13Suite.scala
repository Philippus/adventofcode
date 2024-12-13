package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day13.*
import munit.FunSuite

class Day13Suite extends FunSuite:
  test("finds fewest tokens for a claw machine"):
    val clawMachine = ClawMachine(Button(94, 34), Button(22, 67), Prize(8400, 5400))
    assertEquals(minCost(clawMachine), Some(280))

  test("finds fewest tokens for the sample input"):
    assertEquals(fewestTokensToWinAllPrizes(importSampleLines()), BigDecimal(480))

  test("finds fewest tokens for the input"):
    assertEquals(fewestTokensToWinAllPrizes(importLines()), BigDecimal(35729))

  test("finds fewest tokens for a claw machine through equations"):
    val clawMachine = ClawMachine(Button(94, 34), Button(22, 67), Prize(8400, 5400))
    assertEquals(minCostThroughEqs(clawMachine), Some(BigDecimal(280)))

  test("finds fewest tokens for the input part two"):
    assertEquals(fewestTokensToWinAllPrizesPartTwo(importLines()), BigDecimal(88584689879723L))

  def importSampleLines(): List[List[String]] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList.sliding(3, 4).toList
end Day13Suite
