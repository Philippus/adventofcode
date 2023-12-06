package adventofcode2015

import scala.io.Source
import scala.util.Using

object Day08:
  def numCharactersInMemory(str: String): Int =
    str
      .replaceAll("\\\\x[0-9a-fA-F][0-9a-fA-F]|\\\\\\\\|\\\\\"", "r")
      .length - 2

  def numCharactersAfterEncoding(str: String): Int =
    // no need to do actual replacing to get the answer
    str
      .replace(""""""", "aa")
      .replace("""\""", "aa")
      .++("aa")
      .length

  def calculateValueForFile: Int =
    Using.resource(Source.fromResource("2015/day08input.txt")):
      _.getLines().map(line => line.length - numCharactersInMemory(line)).toSeq.sum

  def calculateValueForFilePartTwo: Int =
    Using.resource(Source.fromResource("2015/day08input.txt")):
      _.getLines().map(line => numCharactersAfterEncoding(line) - line.length).toSeq.sum
end Day08
