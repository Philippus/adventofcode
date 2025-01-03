package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day17.*
import munit.FunSuite

class Day17Suite extends FunSuite:
  test("draws a slice"):
    val lines = importSampleLines()
    println(drawSlice(parseToCubes(lines), 0))
    println(drawSlice(simulateCycles(parseToCubes(lines), 1), -1))
    println(drawSlice(simulateCycles(parseToCubes(lines), 1), 0))
    println(drawSlice(simulateCycles(parseToCubes(lines), 1), 1))
    println(drawSlice(simulateCycles(parseToCubes(lines), 2), -2))
    println(drawSlice(simulateCycles(parseToCubes(lines), 2), -1))
    println(drawSlice(simulateCycles(parseToCubes(lines), 2), 0))
    println(drawSlice(simulateCycles(parseToCubes(lines), 2), 1))
    println(drawSlice(simulateCycles(parseToCubes(lines), 2), 2))

  test("counts active cubes for the sample"):
    val lines = importSampleLines()
    assertEquals(countActiveCubes(simulateCycles(parseToCubes(lines), 6)), 112)

  test("counts active cubes for the input"):
    val lines = importLines()
    assertEquals(countActiveCubes(simulateCycles(parseToCubes(lines), 6)), 213)

  test("draws a 4-dimensional slice"):
    val lines = importSampleLines()
    println(drawHypercubeSlice(parseToHypercubes(lines), 0, 0))
    println(drawHypercubeSlice(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 1), -1, -1))
    println(drawHypercubeSlice(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 1), 0, -1))
    println(drawHypercubeSlice(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 1), 1, -1))
    println(drawHypercubeSlice(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 1), -1, 0))
    println(drawHypercubeSlice(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 1), 0, 0))
    println(drawHypercubeSlice(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 1), 1, 0))
    println(drawHypercubeSlice(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 1), -1, 1))
    println(drawHypercubeSlice(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 1), 0, 1))
    println(drawHypercubeSlice(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 1), 1, 1))

  test("counts active hypercubes for the sample"):
    val lines = importSampleLines()
    assertEquals(countActiveCubes(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 6)), 848)

  test("counts active hypercubes for the input"):
    val lines = importLines()
    assertEquals(countActiveCubes(simulateCyclesIn4Dimensions(parseToHypercubes(lines), 6)), 1624)

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toVector
end Day17Suite
