package adventofcode2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day16:
  case class Sample(before: Vector[Int], instruction: Vector[Int], after: Vector[Int])

  def applyInstruction(opcode: String, inputA: Int, inputB: Int, inputC: Int, registers: Map[Int, Int]): Map[Int, Int] =
    opcode match
      case "addr" => registers.updated(inputC, registers(inputA) + registers(inputB))
      case "addi" => registers.updated(inputC, registers(inputA) + inputB)
      case "mulr" => registers.updated(inputC, registers(inputA) * registers(inputB))
      case "muli" => registers.updated(inputC, registers(inputA) * inputB)
      case "banr" => registers.updated(inputC, registers(inputA) & registers(inputB))
      case "bani" => registers.updated(inputC, registers(inputA) & inputB)
      case "borr" => registers.updated(inputC, registers(inputA) | registers(inputB))
      case "bori" => registers.updated(inputC, registers(inputA) | inputB)
      case "setr" => registers.updated(inputC, registers(inputA))
      case "seti" => registers.updated(inputC, inputA)
      case "gtir" => registers.updated(inputC, if inputA > registers(inputB) then 1 else 0)
      case "gtri" => registers.updated(inputC, if registers(inputA) > inputB then 1 else 0)
      case "gtrr" => registers.updated(inputC, if registers(inputA) > registers(inputB) then 1 else 0)
      case "eqir" => registers.updated(inputC, if inputA == registers(inputB) then 1 else 0)
      case "eqri" => registers.updated(inputC, if registers(inputA) == inputB then 1 else 0)
      case "eqrr" => registers.updated(inputC, if registers(inputA) == registers(inputB) then 1 else 0)

  def testSample(sample: Sample): Vector[String] =
    val inputA    = sample.instruction(1)
    val inputB    = sample.instruction(2)
    val inputC    = sample.instruction(3)
    val registers = sample.before.zipWithIndex.map(_.swap).toMap
    val expected  = sample.after.zipWithIndex.map(_.swap).toMap
    val opcodes   = Vector(
      "addr",
      "addi",
      "mulr",
      "muli",
      "banr",
      "bani",
      "borr",
      "bori",
      "setr",
      "seti",
      "gtir",
      "gtri",
      "gtrr",
      "eqir",
      "eqri",
      "eqrr"
    )
    opcodes.filter: opcode =>
      expected == applyInstruction(opcode, inputA, inputB, inputC, registers)

  def samplesBehavingLikeThreeOrMoreOpcodes(samples: Vector[Sample]): Int =
    samples.count(testSample(_).size >= 3)

  def determineOpcodesAndExecuteTestProgram(samples: Vector[Sample], instructions: Vector[Vector[Int]]): Int =
    @tailrec
    def loop(
        instructions: Vector[Vector[Int]],
        registers: Map[Int, Int],
        lookupTable: Map[Int, String]
    ): Map[Int, Int] =
      if instructions.isEmpty then
        registers
      else
        val instruction = instructions.head
        val newReg      =
          applyInstruction(
            lookupTable(instruction(0)),
            instruction(1),
            instruction(2),
            instruction(3),
            registers: Map[Int, Int]
          )
        loop(instructions.tail, newReg, lookupTable)

    val lookupTable: mutable.Map[Int, String] = mutable.Map.empty[Int, String]
    val opcodesStrings                        = mutable.ListBuffer(
      "addr",
      "addi",
      "mulr",
      "muli",
      "banr",
      "bani",
      "borr",
      "bori",
      "setr",
      "seti",
      "gtir",
      "gtri",
      "gtrr",
      "eqir",
      "eqri",
      "eqrr"
    )
    while lookupTable.size != opcodesStrings.length do
      val opcodesToCandidates = samples.filterNot(sample => lookupTable.contains(sample.instruction.head)).map:
        sample => (sample.instruction.head, testSample(sample).filterNot(s => lookupTable.exists(_._2 == s)))
      .groupBy(_._1).map(c => (c._1, c._2.distinct))
      val unique              = opcodesToCandidates.find: otc =>
        otc._2.head._2.size == 1
      lookupTable.update(unique.get._1, unique.get._2.head._2.head)
    loop(instructions, Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0), lookupTable.toMap)(0)

  def handleLines(lines: Seq[String]): (Vector[Sample], Vector[Vector[Int]]) =
    val lastAfter   = lines.lastIndexWhere(_.startsWith("After:  "))
    val samples     = lines.take(lastAfter + 2).grouped(4).map:
      case List(s"Before: [$before]", s"$instruction", s"After:  [$after]", "") =>
        Sample(
          before.split(", ").map(_.toInt).toVector,
          instruction.split(' ').map(_.toInt).toVector,
          after.split(", ").map(_.toInt).toVector
        )
    .toVector
    val testProgram = lines.drop(lastAfter + 2).toVector.filter(_.nonEmpty).map(_.split(' ').map(_.toInt).toVector)
    (samples, testProgram)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day16
