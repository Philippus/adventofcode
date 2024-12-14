package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day18:
  def nextRow(row: String): String =
    s".$row.".sliding(3).toList.map:
      case "^^." | ".^^" | "^.." | "..^" => "^"
      case _                             => "."
    .mkString

  @tailrec
  def generateMap(row: String, rowsLeft: Int, acc: Seq[String] = Seq.empty): Seq[String] =
    if rowsLeft == 0 then
      acc
    else
      val next = nextRow(row)
      if acc.isEmpty then
        generateMap(next, rowsLeft - 2, Seq(row, next))
      else
        generateMap(next, rowsLeft - 1, acc :+ next)

  @tailrec
  def countSafeTiles(row: String, rowsLeft: Int, acc: Long = 0L): Long =
    if rowsLeft == 0 then
      acc
    else
      val next = nextRow(row)
      countSafeTiles(next, rowsLeft - 1, acc + row.count(_.==('.')))

  def importLines(): String =
    Using.resource(Source.fromResource(s"2016/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq.head
end Day18
