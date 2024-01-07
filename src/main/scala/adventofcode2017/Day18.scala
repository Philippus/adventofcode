package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day18:
  def followInstructions(instructions: Seq[String]) =
    @tailrec
    def loop(currentPos: Long, registers: Map[String, Long]): Long =
      if currentPos >= instructions.length then
        -1
      else
        instructions(currentPos.toInt) match
          case s"snd $a"                                                                 =>
            loop(currentPos + 1, registers + ("snd" -> registers.getOrElse(a, 0)))
          case s"set $a $b" if b.toLongOption.nonEmpty                                   =>
            loop(currentPos + 1, registers + (a -> b.toInt))
          case s"set $a $b"                                                              =>
            loop(currentPos + 1, registers + (a -> registers.getOrElse(b, 0)))
          case s"add $a $b" if b.toLongOption.nonEmpty                                   =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) + b.toLong)))
          case s"add $a $b"                                                              =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) + registers.getOrElse(b, 0L))))
          case s"mul $a $b" if b.toLongOption.nonEmpty                                   =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) * b.toLong)))
          case s"mul $a $b"                                                              =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) * registers.getOrElse(b, 0L))))
          case s"mod $a $b" if b.toLongOption.nonEmpty                                   =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) % b.toLong)))
          case s"mod $a $b"                                                              =>
            loop(currentPos + 1, registers + (a -> (registers.getOrElse(a, 0L) % registers.getOrElse(b, 0L))))
          case s"rcv $a" if registers.getOrElse("snd", 0) == 0                           =>
            loop(currentPos + 1, registers)
          case s"rcv $a"                                                                 =>
            registers.getOrElse("snd", 0)
          case s"jgz $a $b" if b.toLongOption.nonEmpty && registers.getOrElse(a, 0L) > 0 =>
            loop(currentPos + b.toLong, registers)
          case s"jgz $a $b" if b.toLongOption.isEmpty && registers.getOrElse(a, 0L) > 0  =>
            loop(currentPos + registers.getOrElse(b, 0L), registers)
          case s"jgz $a"                                                                 =>
            loop(currentPos + 1, registers)
    loop(0L, Map.empty)

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2017/day18input.txt")):
      _.getLines().toSeq
end Day18
