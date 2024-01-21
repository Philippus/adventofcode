package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12:
  def executeCode(instructions: Seq[String], registers: Map[Char, Long] = Map.empty): Map[Char, Long] =
    @tailrec
    def loop(nextInstruction: Int, registers: Map[Char, Long]): Map[Char, Long] =
      if nextInstruction >= instructions.length then
        registers
      else
        val instruction = instructions(nextInstruction)
        instruction match
          case s"inc $x"                              => loop(nextInstruction + 1, registers + (x.head -> (registers.getOrElse(x.head, 0L) + 1L)))
          case s"dec $x"                              => loop(nextInstruction + 1, registers + (x.head -> (registers.getOrElse(x.head, 0L) - 1L)))
          case s"jnz $x $y" if x.toIntOption.nonEmpty =>
            if x.toInt != 0 then
              loop(nextInstruction + y.toInt, registers)
            else
              loop(nextInstruction + 1, registers)
          case s"jnz $x $y"                           =>
            if registers.getOrElse(x.head, 0) != 0 then
              loop(nextInstruction + y.toInt, registers)
            else
              loop(nextInstruction + 1, registers)
          case s"cpy $x $y" if x.toIntOption.nonEmpty =>
            loop(nextInstruction + 1, registers + (y.head -> x.toLong))
          case s"cpy $x $y"                           =>
            loop(nextInstruction + 1, registers + (y.head -> registers.getOrElse(x.head, 0L)))
    loop(0, registers)
  def readInputfile(): Seq[String]                                                                    =
    Using.resource(Source.fromResource("2016/day12input.txt")): source =>
      source.getLines().toSeq
end Day12
