package adventofcode2023.day1

import scala.io.Source
import scala.util.Using

object Day1 {
  def calibrationValue(line: String): Int = {
    (for {
      firstDigit <- line.find(_.isDigit)
      lastDigit  <- line.findLast(_.isDigit)
    } yield firstDigit.toString ++ lastDigit.toString).map(_.toInt).get
  }

  def calibrationValuePartTwo(line: String): Int = {
//    val digits                  = Map(
    //      "1" -> "one",
    //      "2" -> "two",
    //      "3" -> "three",
    //      "4" -> "four",
    //      "5" -> "five",
    //      "6" -> "six",
    //      "7" -> "seven",
    //      "8" -> "eight",
    //      "9" -> "nine"
    //    )
    //    val candidates: Seq[String] = digits.keys.toSeq ++ digits.values.toSeq
    //
    //    Seq(candidates.map(line.indexOf).filterNot(x => x == -1).min, candidates.map(line.lastIndexOf).max)
    //      .flatMap(x => candidates.filter(line.substring(x).startsWith))
    //      .map(fd => digits.find(_._2 == fd).orElse(digits.find(_._1 == fd)))
    //      .map(_.get._1).mkString.toInt
    val candidates: Seq[String] = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine") ++
      Range.inclusive(1, 9).map(_.toString)

    Seq(candidates.map(line.indexOf).filter(_.>=(0)).min, candidates.map(line.lastIndexOf).max)
      .flatMap(i => candidates.filter(line.substring(i).startsWith))
      .map(d => (candidates.indexOf(d) % 9) + 1).mkString.toInt

//      s.map(x => ((x % 9) + 1).toString).mkString.toInt
//      printlns + 1 )
//      s.flatMap(x => candidates.filter(line.substring(x).startsWith))
//      .map(fd => digits.find(_ == fd).orElse(digits.map(digits.indexOf(_) + 1.toString).find(_ == fd)))
//      .map(_.get).mkString.toInt
  }

  def readCalibrationDocument(calibrationFunction: String => Int): Int = {
    Using.resource(Source.fromResource("day1input.txt")) {
      _.getLines().map(calibrationFunction).sum
    }
  }
}
