package adventofcode2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10:
  def followInstructions(instructions: Seq[String]): Int =
    @tailrec
    def loop(instructions: Seq[String], cycle: Int, registerX: Int, acc: Seq[Int]): Int =
      if acc.length == 6 || instructions.isEmpty then
        acc.zipWithIndex.map((v, i) => (20 + i * 40) * v).sum
      else
        instructions.head match
          case "noop"     =>
            if Seq(20, 60, 100, 140, 180, 220).contains(cycle) then
              loop(instructions.tail, cycle + 1, registerX, acc :+ registerX)
            else
              loop(instructions.tail, cycle + 1, registerX, acc)
          case s"addx $i" =>
            if Seq(19, 20, 59, 60, 99, 100, 139, 140, 179, 180, 219, 220).contains(cycle) then
              loop(instructions.tail, cycle + 2, registerX + i.toInt, acc :+ registerX)
            else
              loop(instructions.tail, cycle + 2, registerX + i.toInt, acc)

    loop(instructions, 1, 1, Seq.empty)

  def drawLettersOnCRT(instructions: Seq[String]): Unit =
    @tailrec
    def loop(instructions: Seq[String], cycle: Int, registerX: Int, position: Int, acc: String): Unit =
      if instructions.isEmpty then
        println(acc.grouped(40).mkString("\n"))
      else
        instructions.head match
          case "noop"     =>
            val char = if (Seq(registerX - 1, registerX, registerX + 1).contains(position % 40)) '#' else '.'
            loop(instructions.tail, cycle + 1, registerX, position + 1, acc :+ char)
          case s"addx $i" =>
            val char1 = if (Seq(registerX - 1, registerX, registerX + 1).contains(position % 40)) '#' else '.'
            val char2 = if (Seq(registerX - 1, registerX, registerX + 1).contains((position + 1) % 40)) '#' else '.'
            loop(
              instructions.tail,
              cycle + 2,
              registerX + i.toInt,
              position + 2,
              acc ++ (char1.toString + char2.toString)
            )

    loop(instructions, 1, 1, 0, "")

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2022/day10input.txt")): source =>
      source.getLines().toSeq
end Day10
