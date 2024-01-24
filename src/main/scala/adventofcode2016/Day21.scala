package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day21:
  def scramble(instructions: Seq[String], s: String): String =
    @tailrec
    def loop(instructions: Seq[String], s: String): String =
      if instructions.isEmpty then
        s
      else
        instructions.head match
          case s"swap position $x with position $y"     =>
            val sX = s(x.toInt)
            val sY = s(y.toInt)
            loop(instructions.tail, s.updated(x.toInt, sY).updated(y.toInt, sX))
          case s"swap letter $x with letter $y"         =>
            loop(instructions.tail, s.replace(x.head, '_').replace(y.head, x.head).replace('_', y.head))
          case s"rotate left $x step$a"                 =>
            loop(instructions.tail, s.drop(x.toInt) ++ s.take(x.toInt))
          case s"rotate right $x step$a"                =>
            loop(instructions.tail, s.takeRight(x.toInt) ++ s.dropRight(x.toInt))
          case s"rotate based on position of letter $x" =>
            val posC      = s.indexOf(x.head)
            val rotations = (1 + posC + (if posC >= 4 then 1 else 0)) % s.length
            loop(instructions.tail, s.takeRight(rotations) ++ s.dropRight(rotations))
          case s"reverse positions $x through $y"       =>
            val before = s.slice(0, x.toInt)
            val slice  = s.slice(x.toInt, y.toInt + 1)
            val after  = s.substring(y.toInt + 1)
            loop(instructions.tail, before ++ slice.reverse ++ after)
          case s"move position $x to position $y"       =>
            val c = s.charAt(x.toInt)
            val t = s.replace(c.toString, "")
            loop(instructions.tail, t.substring(0, y.toInt) ++ c.toString ++ t.substring(y.toInt))
    loop(instructions, s)

  def findPassword(instructions: Seq[String], scrambled: String): String =
    scrambled.permutations.find(scramble(instructions, _) == scrambled).get

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day21input.txt")): source =>
      source.getLines().toSeq
end Day21
