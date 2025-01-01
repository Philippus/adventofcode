package adventofcode2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day25:
  def snafuToDec(snafu: String): Long =
    @tailrec
    def loop(snafu: String, acc: Long): Long =
      if snafu.isEmpty then
        acc
      else
        snafu.head match
          case '2' => loop(snafu.tail, acc * 5 + 2)
          case '1' => loop(snafu.tail, acc * 5 + 1)
          case '0' => loop(snafu.tail, acc * 5)
          case '-' => loop(snafu.tail, acc * 5 - 1)
          case '=' => loop(snafu.tail, acc * 5 - 2)

    loop(snafu, 0L)

  def decToSnafu(value: Long): String =
    @tailrec
    def loop(value: Long, acc: String): String =
      if value == 0L then
        acc
      else
        val m     = value % 5
        val d     = if m > 2 then m - 5 else m
        val digit = d match
          case 2  => '2'
          case 1  => '1'
          case 0  => '0'
          case -1 => '-'
          case -2 => '='
        loop((value - d) / 5, digit +: acc)

    loop(value, "")

  def sumSnafus(snafus: Seq[String]): String =
    val decs = snafus.map(snafuToDec)
    decToSnafu(decs.sum)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day25
