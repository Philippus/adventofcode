package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day16:
  def spin(s: String, i: Int): String =
    s.takeRight(i) ++ s.dropRight(i)

  def exchange(s: String, posA: Int, posB: Int): String =
    val valA = s(posA)
    val valB = s(posB)
    s.updated(posA, valB).updated(posB, valA)

  def partner(s: String, programA: Char, programB: Char): String =
    val posA = s.indexOf(programA)
    val posB = s.indexOf(programB)
    exchange(s, posA, posB)

  def dance(initialSeq: Seq[String], programs: String, dances: Long): String =
    @tailrec
    def loop(seq: Seq[String], acc: String, dancesLeft: Long): String =
      if seq.isEmpty && dancesLeft == 0 then
        acc
      else if seq.isEmpty then
        loop(initialSeq, acc, dancesLeft - 1)
      else
        seq.head match
          case s"s$i"                  => loop(seq.tail, spin(acc, i.toInt), dancesLeft)
          case s"x$posA/$posB"         => loop(seq.tail, exchange(acc, posA.toInt, posB.toInt), dancesLeft)
          case s"p$programA/$programB" => loop(seq.tail, partner(acc, programA.head, programB.head), dancesLeft)

    loop(initialSeq, programs, dances - 1)

  def findCycleLength(initialSeq: Seq[String], programs: String): Long =
    @tailrec
    def loop(seq: Seq[String], acc: String, dance: Long): Long =
      if seq.isEmpty then
        if acc == programs then
          dance
        else
          loop(initialSeq, acc, dance + 1)
      else
        seq.head match
          case s"s$i"                  => loop(seq.tail, spin(acc, i.toInt), dance)
          case s"x$posA/$posB"         => loop(seq.tail, exchange(acc, posA.toInt, posB.toInt), dance)
          case s"p$programA/$programB" => loop(seq.tail, partner(acc, programA.head, programB.head), dance)
    loop(initialSeq, programs, 1)

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2017/day16input.txt")):
      _.getLines().toSeq.head.split(',')
end Day16
