package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

import Day10.*

object Day14:
  def hashToBits(s: String): String =
    s.map(char =>
      val i = Integer.parseInt(char.toString, 16)
      val b = i.toBinaryString
      "0" * (4 - b.length) ++ b
    ).mkString
end Day14
