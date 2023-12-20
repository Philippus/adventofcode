package adventofcode2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day05:
  case class Stack(id: Int, crates: Seq[String])
  case class Instruction(amount: Int, source: Int, target: Int)

  @tailrec
  def followInstructions(
      stacks: Set[Stack],
      instructions: Seq[Instruction],
      crateMover9001: Boolean = false
  ): Set[Stack] =
    instructions match
      case Seq()               => stacks
      case instruction +: rest =>
        val source    = stacks.find(_.id == instruction.source).get
        val target    = stacks.find(_.id == instruction.target).get
        val toMove    = source.crates.take(instruction.amount)
        val newSource = source.copy(crates = source.crates.drop(instruction.amount))
        val newTarget = target.copy(crates = (if crateMover9001 then toMove else toMove.reverse) ++ target.crates)
        val newStacks = stacks.-(source).-(target).+(newSource).+(newTarget)
        followInstructions(newStacks, rest, crateMover9001)
  def handleLine(line: String): Instruction =
    line match
      case s"move $amount from $source to $target" => Instruction(amount.toInt, source.toInt, target.toInt)

  def importLines(): Seq[Instruction] =
    Using.resource(Source.fromResource("2022/day05input.txt")): source =>
      val lines  = source.getLines().toSeq
      val splits = lines.splitAt(lines.indexWhere(_.isEmpty) + 1)
      splits._2.map(handleLine)
end Day05
