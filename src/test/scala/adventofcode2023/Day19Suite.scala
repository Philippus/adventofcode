package adventofcode2023

import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

import adventofcode2023.Day19.*
import munit.FunSuite

class Day19Suite extends FunSuite:
  test("should work for the example"):
    val inWorkflow = Workflow("in", Seq(Rule("s", "<", 1351, "px")), "qqz")
    val workflows  = Set(
      Workflow("px", Seq(Rule("a", "<", 2006, "qkq"), Rule("m", ">", 2090, "A")), "rfg"),
      Workflow("pv", Seq(Rule("a", ">", 1716, "R")), "A"),
      Workflow("lnx", Seq(Rule("m", ">", 1548, "A")), "A"),
      Workflow("rfg", Seq(Rule("s", "<", 537, "gd"), Rule("x", ">", 2440, "R")), "A"),
      Workflow("qs", Seq(Rule("s", ">", 3448, "A")), "lnx"),
      Workflow("qkq", Seq(Rule("x", "<", 1416, "A")), "crn"),
      Workflow("crn", Seq(Rule("x", ">", 2662, "A")), "R"),
      Workflow("in", Seq(Rule("s", "<", 1351, "px")), "qqz"),
      Workflow("qqz", Seq(Rule("s", ">", 2770, "qs"), Rule("m", "<", 1801, "hdj")), "R"),
      Workflow("gd", Seq(Rule("a", ">", 3333, "R")), "R"),
      Workflow("hdj", Seq(Rule("m", ">", 838, "A")), "pv"),
      Workflow("A", Seq.empty, "A"),
      Workflow("R", Seq.empty, "R")
    )
    val part       = Part(787, 2655, 1222, 2876)
    val parts      = Set(
      Part(787, 2655, 1222, 2876),
      Part(1679, 44, 2067, 496),
      Part(2036, 264, 79, 2244),
      Part(2461, 1339, 466, 291),
      Part(2127, 1623, 2188, 1013)
    )
    assertEquals(parts.map(part => applyWorkflow(part, inWorkflow, workflows)).sum, 19114)

  test("should work for the input file"):
    val (workflows, parts) = importLines()
    assertEquals(parts.map(part => applyWorkflow(part, workflows.find(_.id == "in").get, workflows)).sum, 489392)

  test("should be able to import the sample file"):
    val (workflows, parts) = importLinesSample()
    assertEquals(parts.map(part => applyWorkflow(part, workflows.find(_.id == "in").get, workflows)).sum, 19114)
  def importLinesSample(): (Set[Workflow], Seq[Part]) =
    Using.resource(Source.fromResource("2023/day19sampleinput.txt")): source =>
      val lines                      = source.getLines().toSeq
      val (workflowLines, partLines) = lines.splitAt(lines.indexWhere(_.isEmpty))
      (workflowLines.map(handleWorkflowLine).toSet ++ terminators, partLines.filter(_.nonEmpty).map(handlePartLine))
end Day19Suite
