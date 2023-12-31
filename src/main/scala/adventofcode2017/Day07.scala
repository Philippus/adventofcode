package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day07:
  case class Program(name: String, weight: Int, programsAbove: Seq[String])

  def weightOfProgram(name: String, programs: Seq[Program]): Int =
    val program = programs.find(_.name == name).get
    program.programsAbove match
      case Seq() => program.weight
      case _     => program.programsAbove.map(name => weightOfProgram(name, programs)).sum + program.weight

  def determineBottomProgram(programs: Seq[Program]): Option[Program] =
    val programsAbove = programs.flatMap(_.programsAbove)
    programs.find(program => !programsAbove.contains(program.name) && program.programsAbove.nonEmpty)

  @tailrec
  def findExpectedWeightOfUnbalancedProgram(name: String, programs: Seq[Program], expectedWeight: Int): Int =
    val program = programs.find(_.name == name).get
    program.programsAbove match
      case Seq() => expectedWeight
      case _     =>
        val programsAbove            = program.programsAbove
        val programsAboveWithWeights =
          program.programsAbove.map(name => weightOfProgram(name, programs)).zip(programsAbove)
        val groupsWithSizes          = programsAboveWithWeights.groupBy(_._1).map(group => (group, group._2.length))
        val newExpectedWeight        = groupsWithSizes.toSeq.minBy(-_._2)._1._1
        if groupsWithSizes.size > 1 then
          findExpectedWeightOfUnbalancedProgram(
            groupsWithSizes.toSeq.minBy(_._2)._1._2.head._2,
            programs,
            newExpectedWeight
          )
        else
          expectedWeight - programsAboveWithWeights.map(_._1).sum

  def handleLine(line: String): Program =
    line match
      case s"$name ($weight) -> $programsAbove" =>
        Program(name, weight.toInt, programsAbove.split(", ").map(_.trim))
      case s"$name ($weight)"                   =>
        Program(name, weight.toInt, Seq.empty)

  def readInputFile(): Seq[Program] =
    Using.resource(Source.fromResource("2017/day07input.txt")):
      _.getLines().toSeq.map(handleLine)
end Day07
