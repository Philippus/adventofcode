package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day17:
  @tailrec
  def runProgram(
      program: Seq[Int],
      regA: Int,
      regB: Int,
      regC: Int,
      iP: Int = 0,
      out: Seq[Int] = Seq.empty[Int]
  ): (Int, Int, Int, Seq[Int]) =
    def comboOperand(operand: Int, regA: Int, regB: Int, regC: Int): Int =
      operand match
        case 0 | 1 | 2 | 3 => operand
        case 4             => regA
        case 5             => regB
        case 6             => regC
        case 7             => ???

    if iP >= program.length then
      (regA, regB, regC, out)
    else
      (program(iP), program(iP + 1)) match
        case (0, operand) => // adv
          val value: Int = (regA / math.pow(2, comboOperand(operand, regA, regB, regC))).toInt
          runProgram(program, value, regB, regC, iP + 2, out)
        case (1, operand) => // bxl
          val value: Int = regB ^ operand
          runProgram(program, regA, value, regC, iP + 2, out)
        case (2, operand) => // bst
          val value: Int = comboOperand(operand, regA, regB, regC) % 8
          runProgram(program, regA, value, regC, iP + 2, out)
        case (3, operand) => // jnz
          if regA == 0 then
            runProgram(program, regA, regB, regC, iP + 2, out)
          else
            runProgram(program, regA, regB, regC, operand, out)
        case (4, operand) => // bxc
          val value: Int = regB ^ regC
          runProgram(program, regA, value, regC, iP + 2, out)
        case (5, operand) => // out
          val value: Int = comboOperand(operand, regA, regB, regC) % 8
          runProgram(program, regA, regB, regC, iP + 2, out :+ value)
        case (6, operand) => // bdv
          val value: Int = (regA / math.pow(2, comboOperand(operand, regA, regB, regC))).toInt
          runProgram(program, regA, value, regC, iP + 2, out)
        case (7, operand) => // cdv
          val value: Int = (regA / math.pow(2, comboOperand(operand, regA, regB, regC))).toInt
          runProgram(program, regA, regB, value, iP + 2, out)

  @tailrec
  def runProgramBigInt(
      program: Seq[Int],
      regA: BigInt,
      regB: BigInt,
      regC: BigInt,
      iP: Int = 0,
      out: Seq[Int] = Seq.empty[Int]
  ): (BigInt, BigInt, BigInt, Seq[Int]) =
    def comboOperand(operand: BigInt, regA: BigInt, regB: BigInt, regC: BigInt): BigInt =
      operand match
        case 0 | 1 | 2 | 3 => operand
        case 4             => regA
        case 5             => regB
        case 6             => regC
        case 7             => ???

    if iP >= program.length then
      (regA, regB, regC, out)
    else
      (program(iP), program(iP + 1)) match
        case (0, operand) => // adv
          val value: BigInt = regA / BigInt(2).pow(comboOperand(operand, regA, regB, regC).toInt)
          runProgramBigInt(program, value, regB, regC, iP + 2, out)
        case (1, operand) => // bxl
          val value: BigInt = regB ^ operand
          runProgramBigInt(program, regA, value, regC, iP + 2, out)
        case (2, operand) => // bst
          val value: BigInt = comboOperand(operand, regA, regB, regC) % 8
          runProgramBigInt(program, regA, value, regC, iP + 2, out)
        case (3, operand) => // jnz
          if regA == 0 then
            runProgramBigInt(program, regA, regB, regC, iP + 2, out)
          else
            runProgramBigInt(program, regA, regB, regC, operand, out)
        case (4, operand) => // bxc
          val value: BigInt = regB ^ regC
          runProgramBigInt(program, regA, value, regC, iP + 2, out)
        case (5, operand) => // out
          val value: BigInt = comboOperand(operand, regA, regB, regC) % 8
          runProgramBigInt(program, regA, regB, regC, iP + 2, out :+ value.toInt)
        case (6, operand) => // bdv
          val value: BigInt = regA / BigInt(2).pow(comboOperand(operand, regA, regB, regC).toInt)
          runProgramBigInt(program, regA, value, regC, iP + 2, out)
        case (7, operand) => // cdv
          val value: BigInt = regA / BigInt(2).pow(comboOperand(operand, regA, regB, regC).toInt)
          runProgramBigInt(program, regA, regB, value, iP + 2, out)

  def findQuineAdv(program: Seq[Int]): Long =
    @tailrec
    def loop(candidates: Seq[BigInt], digits: Int): Long =
      if digits > program.length then
        candidates.min.toLong
      else
        val newCandidates = (for
          candidate <- candidates.flatMap(c => c * 8 - 7 to c * 8 + 7)
          if candidate > 0
          if runProgramBigInt(program, candidate, 0, 0)._4.endsWith(program.takeRight(digits))
        yield candidate).distinct
        loop(newCandidates, digits + 1)

    loop(BigInt(0).to(BigInt(7)), 1)
    // observed that regA = regA / 8 == 0 in the final run of the program, so regA would have been a digit from 0 to 7

  def findQuine(program: Seq[Int]): Int =
    var found = false
    var i     = 0
    while !found && i <= Int.MaxValue do
      if runProgram(program, i, 0, 0)._4 == program then
        found = true
      else
        i += 1
    i

  def initialize(lines: Seq[String]): (Int, Int, Int, Seq[Int]) =
    val init = lines.filter(_.nonEmpty).map:
      case s"Register A: $a"    => a
      case s"Register B: $b"    => b
      case s"Register C: $c"    => c
      case s"Program: $program" => program
    .filter(_.nonEmpty)
    (init.head.toInt, init(1).toInt, init(2).toInt, init(3).split(',').toSeq.map(_.toInt))

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day17
