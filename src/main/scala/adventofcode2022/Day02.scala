package adventofcode2022

import scala.io.Source
import scala.util.Using

object Day02:
  def handleLine(s: String): Int =
    s match
      case "A X" => 1 + 3
      case "A Y" => 2 + 6
      case "A Z" => 3 + 0
      case "B X" => 1 + 0
      case "B Y" => 2 + 3
      case "B Z" => 3 + 6
      case "C X" => 1 + 6
      case "C Y" => 2 + 0
      case "C Z" => 3 + 3

  def handleLinePartTwo(s: String): Int =
    s match
      case "A X" => 3 + 0
      case "A Y" => 1 + 3
      case "A Z" => 2 + 6
      case "B X" => 1 + 0
      case "B Y" => 2 + 3
      case "B Z" => 3 + 6
      case "C X" => 2 + 0
      case "C Y" => 3 + 3
      case "C Z" => 1 + 6
  def calculateSumForImportFile(): Int =
    importLines().map(handleLine).sum

  def calculateSumForImportFilePartTwo(): Int =
    importLines().map(handleLinePartTwo).sum

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2022/day02input.txt")): source =>
      source.getLines().toSeq
end Day02
