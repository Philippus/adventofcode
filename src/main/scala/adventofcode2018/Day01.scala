package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day01:
  def findFirstFrequencyTwice(frequencies: Seq[Int]): Int =
    @tailrec
    def loop(frequenciesLeft: Seq[Int], currentFrequency: Int, frequenciesSeen: Seq[Int]): Int =
      frequenciesLeft match
        case Seq()  =>
          loop(frequencies, currentFrequency, frequenciesSeen)
        case x +: s =>
          val newFrequency = currentFrequency + x
          if frequenciesSeen.contains(newFrequency) then
            newFrequency
          else
            loop(s, newFrequency, frequenciesSeen :+ newFrequency)

    loop(frequencies, 0, Seq.empty)

  def readInputFile(): Seq[Int] =
    Using.resource(Source.fromResource("2018/day01input.txt")):
      _.getLines().toSeq.map(_.toInt)
end Day01
