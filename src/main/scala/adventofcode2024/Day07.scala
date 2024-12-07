package adventofcode2024

import scala.io.Source
import scala.util.Using

object Day07:
  def checkEquation(testValue: Long, numbers: List[Long], withConcat: Boolean): Boolean =
    numbers match {
      case Nil                =>
        false
      case fst :: Nil         =>
        fst == testValue
      case fst :: snd :: rest =>
        checkEquation(testValue, (fst * snd) :: rest, withConcat) ||
        checkEquation(testValue, (fst + snd) :: rest, withConcat) ||
        (withConcat && checkEquation(testValue, (fst.toString ++ snd.toString).toLong :: rest, withConcat))
    }

  def calibrationResult(lines: Seq[String], withConcat: Boolean = false): Long = lines.map:
    case s"$testValue: $numbers" =>
      if checkEquation(testValue.toLong, (numbers.split(" ").map(_.toLong)).toList, withConcat) then
        testValue.toLong
      else 0L
  .sum

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day07
