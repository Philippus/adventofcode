package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day5:
  def isVowel(c: Char): Boolean =
    Set('a', 'e', 'i', 'o', 'u').contains(c)

  def isNice(str: String): Boolean =
    str.count(c => isVowel(c)) >= 3 &&
      str.sliding(2).toSeq.exists(s => s.head == s.last) &&
      !str.contains("ab") &&
      !str.contains("cd") &&
      !str.contains("pq") &&
      !str.contains("xy")

  def isNaughty(str: String): Boolean = !isNice(str)

  def determineNiceStringsForFile(f: String => Boolean): Int =
    Using.resource(Source.fromResource("2015/day5input.txt")):
      _.getLines().map(line => f(line)).count(_.==(true))

  def isNicePartTwo(str: String): Boolean =
    @tailrec
    def containsPair(str: String): Boolean =
      if str.length < 2
      then false
      else if str.drop(2).contains(str.take(2))
      then true
      else containsPair(str.drop(1))

    val odd  = str.zipWithIndex.filter(_._2 % 2 == 1).map(_._1).mkString
    val even = str.zipWithIndex.filter(_._2 % 2 == 0).map(_._1).mkString
    containsPair(str) &&
    (odd.sliding(2).toSeq.exists(s => s.head == s.last) ||
      even.sliding(2).toSeq.exists(s => s.head == s.last))

  def isNaughtyPartTwo(str: String): Boolean = !isNicePartTwo(str)
end Day5
