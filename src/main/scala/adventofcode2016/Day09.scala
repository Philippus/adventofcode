package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09:
  def decompress(s: String): Int =
    @tailrec
    def loop(s: String, acc: Int): Int =
      if s.indexOf('(') == -1 then
        acc + s.length
      else
        val il = s.indexOf('(')
        val ir = s.indexOf(')')
        s.substring(il + 1, ir) match
          case s"${a}x$b" =>
            loop(
              s.substring(ir + 1 + a.toInt),
              acc + (s.substring(0, il) ++ (s.substring(ir + 1, ir + 1 + a.toInt) * b.toInt)).length
            )
    loop(s, 0)

  def decompressPartTwo(s: String): Long =
    @tailrec
    def loop(s: String, acc: Long): Long =
      if s.indexOf('(') == -1 then
        acc + s.length
      else
        val il = s.indexOf('(')
        val ir = s.indexOf(')')
        s.substring(il + 1, ir) match
          case s"${a}x$b" =>
            loop(
              s.substring(ir + 1, ir + 1 + a.toInt) * b.toInt ++ s.substring(ir + 1 + a.toInt),
              acc + s.substring(0, il).length
            )
    loop(s, 0)

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day09input.txt")): source =>
      source.getLines().toSeq
end Day09
