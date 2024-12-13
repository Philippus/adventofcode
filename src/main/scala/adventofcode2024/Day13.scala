package adventofcode2024

import scala.io.Source
import scala.util.Using

object Day13:
  case class Button(x: BigDecimal, y: BigDecimal)

  case class Prize(x: BigDecimal, y: BigDecimal)

  case class ClawMachine(buttonA: Button, buttonB: Button, prize: Prize)

  def minCost(clawMachine: ClawMachine): Option[Int] =
    (for
      a <- 0 to 100
      b <- 0 to 100
      if (a * clawMachine.buttonA.x + b * clawMachine.buttonB.x) == clawMachine.prize.x
      if (a * clawMachine.buttonA.y + b * clawMachine.buttonB.y) == clawMachine.prize.y
    yield a * 3 + b).minOption

  def minCostThroughEqs(clawMachine: ClawMachine): Option[BigDecimal] =
    val lhs = clawMachine.buttonB.x * clawMachine.buttonA.y - clawMachine.buttonB.y * clawMachine.buttonA.x
    val rhs = clawMachine.prize.x * clawMachine.buttonA.y - clawMachine.prize.y * clawMachine.buttonA.x
    val b   = rhs / lhs
    val a   = (clawMachine.prize.x - clawMachine.buttonB.x * b) / clawMachine.buttonA.x
    if a % 1 == 0 && b % 1 == 0 then Some(a * 3 + b)
    else None

  def toClawMachines(lines: List[String], diff: Long = 0): ClawMachine =
    lines match
      case s"Button A: X+$ax, Y+$ay" :: s"Button B: X+$bx, Y+$by" :: s"Prize: X=$px, Y=$py" :: _ =>
        ClawMachine(
          buttonA = Button(BigDecimal(ax.toInt), BigDecimal(ay.toInt)),
          buttonB = Button(BigDecimal(bx.toInt), BigDecimal(by.toInt)),
          prize = Prize(
            BigDecimal(px.toInt) + BigDecimal(diff),
            BigDecimal(py.toInt) + BigDecimal(diff)
          )
        )

  def fewestTokensToWinAllPrizes(lines: List[List[String]]): BigDecimal =
    val clawMachines = lines.map(line => toClawMachines(line))
    clawMachines.flatMap(minCost).sum

  def fewestTokensToWinAllPrizesPartTwo(lines: List[List[String]]): BigDecimal =
    val clawMachines = lines.map(line => toClawMachines(line, 10000000000000L))
    clawMachines.flatMap(minCostThroughEqs).sum

  def importLines(): List[List[String]] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList.sliding(4, 4).toList
end Day13
