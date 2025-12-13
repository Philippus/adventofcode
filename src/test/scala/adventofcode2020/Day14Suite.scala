package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day14.*
import munit.FunSuite

class Day14Suite extends FunSuite:
  test("executes initialization program for the sample"):
    val input   = importSampleLines()
    val program = parse(input)
    assertEquals(execute(program), 165L)

  test("executes initialization program for the input"):
    val input   = importLines()
    val program = parse(input)
    assertEquals(execute(program), 15403588588538L)

  test("executes initialization program with v2 decoder chip for the sample"):
    val input   = importSample2Lines()
    val program = parse(input)
    assertEquals(executeV2(program), 208L)

  test("executes initialization program with v2 decoder chip for the input"):
    val input   = importLines()
    val program = parse(input)
    assertEquals(executeV2(program), 3260587250457L)

  def importSample2Lines(): String =
    Using.resource(Source.fromResource("2020/day14sample2input.txt")): source =>
      source.mkString

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2020/day14sampleinput.txt")): source =>
      source.mkString
end Day14Suite
