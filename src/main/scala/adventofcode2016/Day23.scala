package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day23:
  def executeCode(instructions: Seq[String], registers: Map[Char, Long] = Map.empty): Map[Char, Long] =
    @tailrec
    def loop(instructions: Seq[String], nextInstruction: Int, registers: Map[Char, Long]): Map[Char, Long] =
      if nextInstruction >= instructions.length then
        registers
      else
        val instruction = instructions(nextInstruction)
        instruction match
          case s"inc $x"                                                       =>
            loop(instructions, nextInstruction + 1, registers + (x.head -> (registers.getOrElse(x.head, 0L) + 1L)))
          case s"dec $x"                                                       =>
            loop(instructions, nextInstruction + 1, registers + (x.head -> (registers.getOrElse(x.head, 0L) - 1L)))
          case s"jnz $x $y" if x.toIntOption.nonEmpty && y.toIntOption.isEmpty =>
            if x.toInt != 0 then
              loop(instructions, nextInstruction + registers.getOrElse(y.head, 0L).toInt, registers)
            else
              loop(instructions, nextInstruction + 1, registers)
          case s"jnz $x $y" if x.toIntOption.nonEmpty                          =>
            if x.toInt != 0 then
              loop(instructions, nextInstruction + y.toInt, registers)
            else
              loop(instructions, nextInstruction + 1, registers)
          case s"jnz $x $y"                                                    =>
            if registers.getOrElse(x.head, 0) != 0 then
              loop(instructions, nextInstruction + y.toInt, registers)
            else
              loop(instructions, nextInstruction + 1, registers)
          case s"cpy $x $y" if y.toIntOption.nonEmpty                          =>
            loop(instructions, nextInstruction + 1, registers)
          case s"cpy $x $y" if x.toIntOption.nonEmpty                          =>
            loop(instructions, nextInstruction + 1, registers + (y.head -> x.toLong))
          case s"cpy $x $y"                                                    =>
            loop(instructions, nextInstruction + 1, registers + (y.head -> registers.getOrElse(x.head, 0L)))
          case s"tgl $x"                                                       =>
            val indexToToggle =
              nextInstruction + (if x.toIntOption.nonEmpty then x.toInt else registers.getOrElse(x.head, 0L).toInt)
            if indexToToggle >= instructions.length then
              loop(instructions, nextInstruction + 1, registers)
            else
              val i       = if x.toIntOption.nonEmpty then
                instructions(indexToToggle)
              else
                instructions(indexToToggle)
              val updated = i match
                case s"inc $x"    => s"dec $x"
                case s"dec $x"    => s"inc $x"
                case s"tgl $x"    => s"inc $x"
                case s"jnz $x $y" => s"cpy $x $y"
                case s"cpy $x $y" => s"jnz $x $y"
              if x.toIntOption.nonEmpty then
                loop(instructions.updated(nextInstruction + x.toInt, updated), nextInstruction + 1, registers)
              else
                loop(
                  instructions.updated(nextInstruction + registers.getOrElse(x.head, 0L).toInt, updated),
                  nextInstruction + 1,
                  registers
                )

    loop(instructions, 0, registers)

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day23input.txt")): source =>
      source.getLines().toSeq
end Day23
