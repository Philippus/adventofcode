package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day19:
  case class Instruction(opcode: String, inputA: Int, inputB: Int, inputC: Int)

  def applyInstruction(instruction: Instruction, registers: Map[Int, Int]): Map[Int, Int] =
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

  def followInstructions(bind: Int, instructions: Vector[Instruction]): Int =
    @tailrec
    def loop(ip: Int, registers: Map[Int, Int]): Map[Int, Int] =
      if ip >= instructions.length then
        registers
      else
        val newReg1 = registers.updated(bind, ip)
        val newReg  = applyInstruction(instructions(ip), newReg1)
        val newIp   = newReg(bind)
        loop(newIp + 1, newReg)

    val registers = 0.to(5).map(_ -> 0).toMap

    loop(0, registers)(0)

  def part2(instructions: Vector[Instruction]): Int =
    val n   = (836 + 22 * instructions(21).inputB + instructions(23).inputB) + 10550400
    val sqn = math.sqrt(n).toInt
    (1 to sqn).filter(n % _ == 0).map(d => d + n / d).sum - (if (sqn * sqn == n) sqn else 0)

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
end Day19
