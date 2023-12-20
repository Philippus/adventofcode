package adventofcode2017

import scala.io.Source
import scala.util.Using

object Day01:
  def sumFor(sequence: String): Int =
    val slides = (sequence :+ sequence.head).sliding(2, 1)
    slides.filter(x => x.head == x.last).map(_.head.asDigit).sum

  def sumHalfwayAround(sequence: String): Int =
    val splits = sequence.splitAt(sequence.length / 2)
    val zipped = splits._1.zip(splits._2)
    zipped.filter(x => x._1 == x._2).map(_._1.asDigit).sum * 2

  def readInputFile: String =
    Using.resource(Source.fromResource("2017/day01input.txt")):
      _.getLines().toSeq.head
end Day01
