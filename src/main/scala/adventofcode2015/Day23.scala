package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day23:
  @tailrec
  def readInstructions(instructions: Seq[String], nextInstruction: Int, a: Int, b: Int): Int =
    println(s"$a, $b, $nextInstruction")
    if nextInstruction >= instructions.length then
      b
    else
      instructions(nextInstruction) match
        case s"hlf a"          =>
          readInstructions(instructions, nextInstruction + 1, a / 2, b)
        case s"hlf b"          =>
          readInstructions(instructions, nextInstruction + 1, a, b / 2)
        case s"tpl a"          =>
          readInstructions(instructions, nextInstruction + 1, a * 3, b)
        case s"tpl b"          =>
          readInstructions(instructions, nextInstruction + 1, a, b * 3)
        case s"inc a"          =>
          readInstructions(instructions, nextInstruction + 1, a + 1, b)
        case s"inc b"          =>
          readInstructions(instructions, nextInstruction + 1, a, b + 1)
        case s"jmp $offset"    =>
          readInstructions(instructions, nextInstruction + offset.toInt, a, b)
        case s"jie a, $offset" =>
          if a % 2 == 0 then readInstructions(instructions, nextInstruction + offset.toInt, a, b)
          else readInstructions(instructions, nextInstruction + 1, a, b)
        case s"jie b, $offset" =>
          if b % 2 == 0 then readInstructions(instructions, nextInstruction + offset.toInt, a, b)
          else readInstructions(instructions, nextInstruction + 1, a, b)
        case s"jio a, $offset" =>
          if a == 1 then readInstructions(instructions, nextInstruction + offset.toInt, a, b)
          else readInstructions(instructions, nextInstruction + 1, a, b)
        case s"jio b, $offset" =>
          if b == 1 then readInstructions(instructions, nextInstruction + offset.toInt, a, b)
          else readInstructions(instructions, nextInstruction + 1, a, b)
  def readInstrucionsFromFile(): Seq[String]                                                 =
    Using.resource(Source.fromResource("2015/day23input.txt")): source =>
      source.getLines().toSeq
end Day23
