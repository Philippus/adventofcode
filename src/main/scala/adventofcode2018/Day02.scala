package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day02:
  def exactlyTwo(s: String): Boolean =
    s.groupBy(identity).exists(_._2.length == 2)

  def exactlyThree(s: String): Boolean =
    s.groupBy(identity).exists(_._2.length == 3)

  def checksum(boxIds: Seq[String]): Int =
    boxIds.map(exactlyTwo).count(_.==(true)) * boxIds.map(exactlyThree).count(_.==(true))

  def commonBetweenStrings(s1: String, s2: String): String =
    @tailrec
    def loop(s1: String, s2: String, pos: Int): String =
      if pos >= s1.length then
        ""
      else if s1.updated(pos, '_') == s2.updated(pos, '_') then
        s1.updated(pos, '_').replace("_", "")
      else
        loop(s1, s2, pos + 1)
    loop(s1, s2, 0)

  def commonBetweenBoxIds(boxIds: Seq[String]): String =
    boxIds.combinations(2).collect(combo => commonBetweenStrings(combo.head, combo.last)).filter(_.nonEmpty).toSeq.head

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2018/day02input.txt")):
      _.getLines().toSeq
end Day02
