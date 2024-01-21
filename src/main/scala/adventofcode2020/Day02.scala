package adventofcode2020

import scala.io.Source
import scala.util.Using

object Day02:
  def validatePassword(candidate: String): Boolean =
    candidate match
      case s"$a-$b $c: $pwd" =>
        val count = pwd.count(_.==(c.head))
        a.toInt <= count && count <= b.toInt

  def validatePasswords(candidates: Seq[String]): Int =
    candidates.map(validatePassword).count(_.==(true))

  def validatePasswordPartTwo(candidate: String): Boolean =
    candidate match
      case s"$a-$b $c: $pwd" =>
        val char = c.head
        (pwd(a.toInt - 1) == char && pwd(b.toInt - 1) != char) ||
        (pwd(a.toInt - 1) != char && pwd(b.toInt - 1) == char)

  def validatePasswordsPartTwo(candidates: Seq[String]): Int =
    candidates.map(validatePasswordPartTwo).count(_.==(true))

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2020/day02input.txt")):
      _.getLines().toSeq
end Day02
