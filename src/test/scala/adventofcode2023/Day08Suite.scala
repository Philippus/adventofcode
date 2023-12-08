package adventofcode2023

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

import adventofcode2023.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("calculates the steps for the example"):
    val instructions = "RL"
    val startNodes = Seq("AAA")
    val nodes = Set(
      Choice("AAA", "BBB", "CCC"),
      Choice("BBB", "DDD", "EEE"),
      Choice("CCC", "ZZZ", "GGG"),
      Choice("DDD", "DDD", "DDD"),
      Choice("EEE", "EEE", "EEE"),
      Choice("GGG", "GGG", "GGG"),
      Choice("ZZZ", "ZZZ", "ZZZ")
    )
    val steps = calculateSteps(instructions, startNodes, nodes, "ZZZ")
    assertEquals(steps, BigInt(2))

  test("calculates the steps for the second example"):
    val instructions = "LLR"
    val startNodes = Seq("AAA")
    val nodes = Set(
      Choice("AAA", "BBB", "BBB"),
      Choice("BBB", "AAA", "ZZZ"),
      Choice("ZZZ", "ZZZ", "ZZZ"),
    )
    val steps = calculateSteps(instructions, startNodes, nodes, "ZZZ")
    assertEquals(steps, BigInt(6))

  test("calculates the ghost steps for the example"):
    Using.resource(Source.fromResource("2023/day08sampleinput.txt")): source =>
      val lines = source.getLines.toSeq
      val instructions = lines.head
      val nodes = lines.drop(2).map(readLine).toSet
      val steps = calculateSteps(instructions, nodes.map(_.source).filter(_.endsWith("A")).toSeq, nodes, "Z")
      assertEquals(steps, BigInt(6))

  test("calculates the steps for the input"):
    assertEquals(readDocument, BigInt(17621))

  test("calculates the ghost steps for the input"):
    assertEquals(readDocumentForGhosts, BigInt(20685524831999L))
end Day08Suite
