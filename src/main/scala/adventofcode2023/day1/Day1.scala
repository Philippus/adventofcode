package adventofcode2023.day1

import scala.io.Source
import scala.util.Using

object Day1:
  def calibrationValue(line: String): Int =
    (for
      firstDigit <- line.find(_.isDigit)
      lastDigit  <- line.findLast(_.isDigit)
    yield firstDigit.toString ++ lastDigit.toString).map(_.toInt).get

  def calibrationValuePartTwo(line: String): Int =
    val candidates: Seq[String] = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine") ++
      Range.inclusive(1, 9).map(_.toString)

    Seq(candidates.map(line.indexOf).filter(_.>=(0)).min, candidates.map(line.lastIndexOf).max)
      .flatMap(i => candidates.filter(line.substring(i).startsWith))
      .map(d => (candidates.indexOf(d) % 9) + 1).mkString.toInt

  /** run on the console like this: scala> import adventofcode2023.day1._ import adventofcode2023.day1._
    *
    * scala> Day1.readCalibrationDocument(Day1.calibrationValue) val res0: Int = ???
    *
    * scala> Day1.readCalibrationDocument(Day1.calibrationValuePartTwo) val res1: Int = ???
    */
  def readCalibrationDocument(calibrationFunction: String => Int): Int =
    Using.resource(Source.fromResource("day1input.txt")):
      _.getLines().map(calibrationFunction).sum

end Day1
