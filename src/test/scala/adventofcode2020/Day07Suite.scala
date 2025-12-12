package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("counts bag colors than can contain one shiny gold bag"):
    val input = importSampleLines()
    val bags  = parse(input)
    assertEquals(findShinyGoldBagHoldingBags(bags), 4L)

  test("counts bag colors than can contain one shiny gold bag"):
    val input = importLines()
    val bags  = parse(input)
    assertEquals(findShinyGoldBagHoldingBags(bags), 208L)

  test("counts bags inside shiny gold bag for the sample"):
    val input = importSampleLines()
    val bags  = parse(input)
    assertEquals(countBagsInsideShinyGoldBag(bags), 32L)

  test("counts bags inside shiny gold bag for the input"):
    val input = importLines()
    val bags  = parse(input)
    assertEquals(countBagsInsideShinyGoldBag(bags), 1664L)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2020/day07sampleinput.txt")): source =>
      source.mkString
end Day07Suite
