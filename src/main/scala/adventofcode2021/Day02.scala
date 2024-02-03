package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day02:

  def findPositionOfSubmarine(course: Seq[String]): Int =
    @tailrec
    def loop(course: Seq[String], forward: Int, depth: Int): Int =
      if course.isEmpty then
        forward * depth
      else
        course.head match
          case s"forward $x" => loop(course.tail, forward + x.toInt, depth)
          case s"down $x"    => loop(course.tail, forward, depth + x.toInt)
          case s"up $x"      => loop(course.tail, forward, depth - x.toInt)

    loop(course, 0, 0)

  def findPositionOfSubmarineWithAim(course: Seq[String]): Int =
    @tailrec
    def loop(course: Seq[String], forward: Int, depth: Int, aim: Int): Int =
      if course.isEmpty then
        forward * depth
      else
        course.head match
          case s"forward $x" => loop(course.tail, forward + x.toInt, depth + x.toInt * aim, aim)
          case s"down $x"    => loop(course.tail, forward, depth, aim + x.toInt)
          case s"up $x"      => loop(course.tail, forward, depth, aim - x.toInt)

    loop(course, 0, 0, 0)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2021/day02input.txt")): source =>
      source.getLines().toSeq
end Day02
