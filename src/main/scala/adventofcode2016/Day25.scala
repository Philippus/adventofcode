package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day25:
  def executeCode(instructions: Seq[String], registers: Map[Char, Long] = Map.empty): String =
    @tailrec
    def loop(nextInstruction: Int, registers: Map[Char, Long], out: String): String =
      if out.length > 100 then
        out
      else
        val instruction = instructions(nextInstruction)
        instruction match
          case s"inc $x"                              =>
            loop(nextInstruction + 1, registers + (x.head -> (registers.getOrElse(x.head, 0L) + 1L)), out)
          case s"dec $x"                              =>
            loop(nextInstruction + 1, registers + (x.head -> (registers.getOrElse(x.head, 0L) - 1L)), out)
          case s"jnz $x $y" if x.toIntOption.nonEmpty =>
            if x.toInt != 0 then
              loop(nextInstruction + y.toInt, registers, out)
            else
              loop(nextInstruction + 1, registers, out)
          case s"jnz $x $y"                           =>
            if registers.getOrElse(x.head, 0) != 0 then
              loop(nextInstruction + y.toInt, registers, out)
            else
              loop(nextInstruction + 1, registers, out)
          case s"cpy $x $y" if x.toIntOption.nonEmpty =>
            loop(nextInstruction + 1, registers + (y.head -> x.toLong), out)
          case s"cpy $x $y"                           =>
            loop(nextInstruction + 1, registers + (y.head -> registers.getOrElse(x.head, 0L)), out)
          case s"out $x" if x.toIntOption.nonEmpty    =>
            loop(nextInstruction + 1, registers, out ++ x)
          case s"out $x"                              =>
            loop(nextInstruction + 1, registers, out ++ registers.getOrElse(x.head, 0L).toString)

    loop(0, registers, "")

  def findLowestPositiveInteger(instructions: Seq[String]): Int =
    var i           = 0
    var signalFound = false
    while (!signalFound)
      i += 1
      if (executeCode(instructions, Map('a' -> i)).startsWith("01" * 50))
        signalFound = true
    i

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day25input.txt")): source =>
      source.getLines().toSeq
end Day25
