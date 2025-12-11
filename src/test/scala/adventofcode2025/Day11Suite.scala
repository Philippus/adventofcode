package adventofcode2025

import scala.io.Source
import scala.util.Using

import adventofcode2025.Day11.*
import munit.FunSuite

class Day11Suite extends FunSuite:
  test("""counts paths from "you" to "out" for the sample"""):
    val devices = parse(importSampleLines())
    assertEquals(countPathsFromYouToOut(devices), 5L)

  test("""counts paths from "you" to "out" for the input"""):
    val devices = parse(importLines())
    assertEquals(countPathsFromYouToOut(devices), 719L)

  test("""counts paths from "you" to "out" with memoization for the sample"""):
    val devices = parse(importSampleLines())
    assertEquals(countPathsFromSrcToDestWithMemoization(devices, "you", "out"), 5L)

  test("""counts paths from "you" to "out" with memoization for the input"""):
    val devices = parse(importLines())
    assertEquals(countPathsFromSrcToDestWithMemoization(devices, "you", "out"), 719L)

  test("""counts paths from "svr" to "out" with memoization for the second sample"""):
    val devices = parse(importSecondSampleLines())
    assertEquals(countPathsFromSrcToDestWithMemoization(devices, "svr", "out"), 8L)

  test("""counts paths from "svr" to "out" visiting both "fft" and "dac" with memoization for the 2nd sample"""):
    val devices = parse(importSecondSampleLines())
    assertEquals(countPathsFromSrcToDestWithMemoizationVisitingFftAndDac(devices), 2L)

  test("""counts paths from "svr" to "out" visiting both "fft" and "dac" with memoization for the input"""):
    val devices = parse(importLines())
    assertEquals(countPathsFromSrcToDestWithMemoizationVisitingFftAndDac(devices), 337433554149492L)

  def importSecondSampleLines(): String =
    Using.resource(Source.fromResource("2025/day11sample2input.txt")): source =>
      source.mkString

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2025/day11sampleinput.txt")): source =>
      source.mkString
end Day11Suite
