package adventofcode2022

import scala.io.Source
import scala.util.Using

import adventofcode2022.Day11.*
import munit.FunSuite

class Day11Suite extends FunSuite:
  test("determines level of monkey business for the sample"):
    val monkeys = handleLines(importSampleLines())
    assertEquals(levelOfMonkeyBusiness(monkeys), 10605L)

  test("determines level of monkey business for the input"):
    val monkeys = handleLines(importLines())
    assertEquals(levelOfMonkeyBusiness(monkeys), 120756L)

  test("determines level of monkey business after 10000 rounds for the sample"):
    val monkeys = handleLines(importSampleLines())
    assertEquals(levelOfMonkeyBusiness(monkeys, true), 2713310158L)

  test("determines level of monkey business after 10000 rounds for the input"):
    val monkeys = handleLines(importLines())
    assertEquals(levelOfMonkeyBusiness(monkeys, true), 39109444654L)

  def importSampleLines(): List[List[String]] =
    Using.resource(
      Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().map(_.trim).toList.grouped(7).toList
end Day11Suite
