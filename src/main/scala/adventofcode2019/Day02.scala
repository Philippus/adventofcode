package adventofcode2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day02:
  @tailrec
  def process(program: List[Int], iP: Int): Int =
    program(iP) match {
      case 99 =>
        program.head
      case 1  =>
        process(program.updated(program(iP + 3), program(program(iP + 1)) + program(program(iP + 2))), iP + 4)
      case 2  =>
        process(program.updated(program(iP + 3), program(program(iP + 1)) * program(program(iP + 2))), iP + 4)
    }

  def restoreGravityAssistProgram(program: List[Int]): Int =
    process(program.updated(1, 12).updated(2, 2), 0)

  def findNounAndVerb(program: List[Int]): Int =
    (for
      noun <- 0 to 99
      verb <- 0 to 99
      if process(program.updated(1, noun).updated(2, verb), 0) == 19690720
    yield 100 * noun + verb)
      .head

  def importLines(): List[Int] =
    Using.resource(Source.fromResource("2019/day02input.txt")):
      _.getLines().toList.head.split(',').toList.map(_.toInt)
end Day02
