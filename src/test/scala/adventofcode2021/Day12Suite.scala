package adventofcode2021

import scala.concurrent.duration.Duration
import scala.io.Source
import scala.util.Using

import adventofcode2021.Day12.*
import munit.FunSuite

class Day12Suite extends FunSuite:
  override val munitTimeout = Duration(500, "s")

  test("counts paths though the cave system that visit small caves at most once for the sample"):
    val edges = parse(importSampleLines())
    assertEquals(calculatePathsThatVisitSmallCavesAtMostOnce(edges), 10)

  test("counts paths though the cave system that visit small caves at most once for the second sample"):
    val edges = parse(importSample2Lines())
    assertEquals(calculatePathsThatVisitSmallCavesAtMostOnce(edges), 19)

  test("counts paths though the cave system that visit small caves at most once for the third sample"):
    val edges = parse(importSample3Lines())
    assertEquals(calculatePathsThatVisitSmallCavesAtMostOnce(edges), 226)

  test("counts paths though the cave system that visit small caves at most once for the input"):
    val edges = parse(importLines())
    assertEquals(calculatePathsThatVisitSmallCavesAtMostOnce(edges), 3563)

  test("counts paths though the cave system that visits a single small cave at most twice for the sample"):
    val edges = parse(importSampleLines())
    assertEquals(calculatePathsThatVisitOneSmallCaveAtMostTwice(edges), 36)

  test("counts paths though the cave system that visits a single small cave at most twice for the second sample"):
    val edges = parse(importSample2Lines())
    assertEquals(calculatePathsThatVisitOneSmallCaveAtMostTwice(edges), 103)

  test("counts paths though the cave system that visits a single small cave at most twice for the third sample"):
    val edges = parse(importSample3Lines())
    assertEquals(calculatePathsThatVisitOneSmallCaveAtMostTwice(edges), 3509)

  test("counts paths though the cave system that visits a single small cave at most twice for the input"):
    val edges = parse(importLines())
    assertEquals(calculatePathsThatVisitOneSmallCaveAtMostTwice(edges), 105453)

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2021/day12sampleinput.txt")): source =>
      source.mkString

  def importSample2Lines(): String =
    Using.resource(Source.fromResource("2021/day12sample2input.txt")): source =>
      source.mkString

  def importSample3Lines(): String =
    Using.resource(Source.fromResource("2021/day12sample3input.txt")): source =>
      source.mkString
end Day12Suite
