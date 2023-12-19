package adventofcode2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day19:
  case class Rule(id: String, comparator: String, compareWith: Int, nextRule: String)

  case class Workflow(id: String, rules: Seq[Rule], otherwise: String)

  case class Part(x: Int, m: Int, a: Int, s: Int) {
    def get(ss: String): Int = ss match
      case "x" => x
      case "m" => m
      case "a" => a
      case "s" => s
  }

  val terminators = Set(Workflow("A", Seq.empty, "A"), Workflow("R", Seq.empty, "R"))

  @tailrec
  def applyWorkflow(part: Part, workflow: Workflow, workflows: Set[Workflow]): Int =
    if workflow.id == "A" then
      part.x + part.m + part.a + part.s
    else if workflow.id == "R" then
      0
    else
      val rules = workflow.rules
      val r     = rules.find { rule =>
        val (id, comparator, compareWith) = (rule.id, rule.comparator, rule.compareWith)
        comparator match
          case ">" => part.get(id) > compareWith
          case "<" => part.get(id) < compareWith
      }
      r match
        case Some(rule) =>
          applyWorkflow(part, workflows.find(_.id == rule.nextRule).get, workflows)
        case None       =>
          applyWorkflow(part, workflows.find(_.id == workflow.otherwise).get, workflows)

  def handleWorkflowLine(s: String): Workflow =
    def handleRuleString(s: String): Rule =
      s match
        case s"$id<$compareWith:$nextRule" => Rule(id, "<", compareWith.toInt, nextRule)
        case s"$id>$compareWith:$nextRule" => Rule(id, ">", compareWith.toInt, nextRule)

    def handleRulesString(s: String): (Seq[Rule], String) =
      val a     = s.split(',')
      val rules = a.init.map(handleRuleString)
      (rules, a.last)

    s match
      case s"$id{$rulesAndOtherwise}" =>
        val (rules, otherwise) = handleRulesString(rulesAndOtherwise)
        Workflow(id, rules, otherwise)

  def handlePartLine(s: String): Part =
    s match
      case s"{x=$x,m=$m,a=$a,s=$s}" => Part(x.toInt, m.toInt, a.toInt, s.toInt)

  def importLines(): (Set[Workflow], Seq[Part]) =
    Using.resource(Source.fromResource("2023/day19input.txt")): source =>
      val lines              = source.getLines().toSeq
      val (workflows, parts) = lines.splitAt(lines.indexWhere(_.isEmpty))
      (workflows.map(handleWorkflowLine).toSet ++ terminators, parts.filter(_.nonEmpty).map(handlePartLine))
end Day19
