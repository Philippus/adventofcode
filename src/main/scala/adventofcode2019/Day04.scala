package adventofcode2019

import scala.io.Source
import scala.util.Using

object Day04:
  def validPassword(password: String, checkLargerGroup: Boolean = false): Boolean =
    password.length == 6 &&
      password.sliding(2).exists(x => x.head == x.last) &&
      password.sliding(2).forall(x => x.head <= x.last) &&
      (!checkLargerGroup || ("x" ++ password ++ "x").sliding(4).exists(x =>
        x(1) == x(2) && x(0) != x(1) && x(3) != x(1)
      ))

  def checkRange(range: String, checkLargerGroup: Boolean = false): Int =
    val (from, to) = range match {
      case s"$from-$to" =>
        (from.toInt, to.toInt)
    }
    (for (i <- from to to)
      yield validPassword(i.toString, checkLargerGroup)).count(identity)

  def importLines(): String =
    Using.resource(Source.fromResource("2019/day04input.txt")):
      _.getLines().toSeq.head
end Day04
