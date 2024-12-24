package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day24.*
import munit.FunSuite

class Day24Suite extends FunSuite:
  test("simulates wires for the tiny sample"):
    val (initialValues, wires) = handleLines(importTinySampleLines())
    assertEquals(simulateGates(initialValues, wires), BigInt(4))

  test("simulates wires for the sample"):
    val (initialValues, wires) = handleLines(importSampleLines())
    assertEquals(simulateGates(initialValues, wires), BigInt(2024))

  test("simulates wires for the input"):
    val (initialValues, wires) = handleLines(importLines())
    assertEquals(simulateGates(initialValues, wires), BigInt(52956035802096L))

  def importTinySampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}tinysampleinput.txt")
    ): source =>
      source.getLines().toList

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day24Suite
