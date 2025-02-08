package adventofcode2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day21:
  case class Instruction(opcode: String, inputA: Int, inputB: Int, inputC: Int)

  def applyInstruction(instruction: Instruction, registers: Map[Int, Long]): Map[Int, Long] =
    instruction.opcode match
      case "addr" =>
        registers.updated(instruction.inputC, registers(instruction.inputA) + registers(instruction.inputB))
      case "addi" => registers.updated(instruction.inputC, registers(instruction.inputA) + instruction.inputB)
      case "mulr" =>
        registers.updated(instruction.inputC, registers(instruction.inputA) * registers(instruction.inputB))
      case "muli" => registers.updated(instruction.inputC, registers(instruction.inputA) * instruction.inputB)
      case "banr" =>
        registers.updated(instruction.inputC, registers(instruction.inputA) & registers(instruction.inputB))
      case "bani" => registers.updated(instruction.inputC, registers(instruction.inputA) & instruction.inputB)
      case "borr" =>
        registers.updated(instruction.inputC, registers(instruction.inputA) | registers(instruction.inputB))
      case "bori" => registers.updated(instruction.inputC, registers(instruction.inputA) | instruction.inputB)
      case "setr" => registers.updated(instruction.inputC, registers(instruction.inputA))
      case "seti" => registers.updated(instruction.inputC, instruction.inputA)
      case "gtir" =>
        registers.updated(instruction.inputC, if instruction.inputA > registers(instruction.inputB) then 1 else 0)
      case "gtri" =>
        registers.updated(instruction.inputC, if registers(instruction.inputA) > instruction.inputB then 1 else 0)
      case "gtrr" => registers.updated(
          instruction.inputC,
          if registers(instruction.inputA) > registers(instruction.inputB) then 1 else 0
        )
      case "eqir" =>
        registers.updated(instruction.inputC, if instruction.inputA == registers(instruction.inputB) then 1 else 0)
      case "eqri" =>
        registers.updated(instruction.inputC, if registers(instruction.inputA) == instruction.inputB then 1 else 0)
      case "eqrr" => registers.updated(
          instruction.inputC,
          if registers(instruction.inputA) == registers(instruction.inputB) then 1 else 0
        )

  def followInstructions(bind: Int, instructions: Vector[Instruction], stopAtFirst: Boolean = false): Long =
    val acc                                                                   = mutable.ArrayBuffer.empty[Long]
    @tailrec
    def loop(ip: Int, registers: Map[Int, Long], newLong: Option[Long]): Long =
      if newLong.nonEmpty && stopAtFirst then
        newLong.get
      else if newLong.nonEmpty && acc.contains(newLong.get) then
        acc.last
      else
        if newLong.nonEmpty then
          acc.addOne(newLong.get)
        val newReg = applyInstruction(instructions(ip), registers.updated(bind, ip))
        loop(
          newReg(bind).toInt + 1,
          newReg,
          if instructions(ip).opcode == "eqrr" then Some(registers(instructions(ip).inputA)) else None
        )

    val registers = 0.to(5).map(_ -> 0L).toMap

    loop(0, registers, None)

  def handleLines(lines: Seq[String]): (Int, Vector[Instruction]) =
    val bind         = lines.head match
      case s"#ip $i" => i.toInt
    val instructions = lines.tail.toVector.map:
      case s"$opcode $a $b $c" => Instruction(opcode, a.toInt, b.toInt, c.toInt)
    (bind, instructions)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day21
