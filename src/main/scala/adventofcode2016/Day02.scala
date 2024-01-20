package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day02:
  def followInstructions(instructions: Seq[String]): String =
    @tailrec
    def loop(instructions: Seq[String], cursor: Int, acc: String): String =
      if instructions.isEmpty then
        acc
      else if instructions.head.isEmpty then
        loop(instructions.tail, cursor, acc + cursor.toString)
      else
        val instruction = instructions.head.head
        instruction match
          case 'U' if Seq(1, 2, 3).contains(cursor) =>
            loop(instructions.head.tail +: instructions.tail, cursor, acc)
          case 'U'                                  =>
            loop(instructions.head.tail +: instructions.tail, cursor - 3, acc)
          case 'D' if Seq(7, 8, 9).contains(cursor) =>
            loop(instructions.head.tail +: instructions.tail, cursor, acc)
          case 'D'                                  =>
            loop(instructions.head.tail +: instructions.tail, cursor + 3, acc)
          case 'L' if Seq(1, 4, 7).contains(cursor) =>
            loop(instructions.head.tail +: instructions.tail, cursor, acc)
          case 'L'                                  =>
            loop(instructions.head.tail +: instructions.tail, cursor - 1, acc)
          case 'R' if Seq(3, 6, 9).contains(cursor) =>
            loop(instructions.head.tail +: instructions.tail, cursor, acc)
          case 'R'                                  =>
            loop(instructions.head.tail +: instructions.tail, cursor + 1, acc)
    loop(instructions, 5, "")

  def followInstructionsPartTwo(instructions: Seq[String]): String =
    @tailrec
    def loop(instructions: Seq[String], cursor: Char, acc: String): String =
      if instructions.isEmpty then
        acc
      else if instructions.head.isEmpty then
        loop(instructions.tail, cursor, acc + cursor.toString)
      else
        val instruction = instructions.head.head
        instruction match
          case 'U' if Seq('1', '2', '5', '4', '9').contains(cursor) =>
            loop(instructions.head.tail +: instructions.tail, cursor, acc)
          case 'U' if cursor == '3'                                 =>
            loop(instructions.head.tail +: instructions.tail, '1', acc)
          case 'U' if cursor == '6'                                 =>
            loop(instructions.head.tail +: instructions.tail, '2', acc)
          case 'U' if cursor == '7'                                 =>
            loop(instructions.head.tail +: instructions.tail, '3', acc)
          case 'U' if cursor == '8'                                 =>
            loop(instructions.head.tail +: instructions.tail, '4', acc)
          case 'U' if cursor == 'A'                                 =>
            loop(instructions.head.tail +: instructions.tail, '6', acc)
          case 'U' if cursor == 'B'                                 =>
            loop(instructions.head.tail +: instructions.tail, '7', acc)
          case 'U' if cursor == 'C'                                 =>
            loop(instructions.head.tail +: instructions.tail, '8', acc)
          case 'U' if cursor == 'D'                                 =>
            loop(instructions.head.tail +: instructions.tail, 'B', acc)
          case 'D' if Seq('5', 'A', 'D', 'C', '9').contains(cursor) =>
            loop(instructions.head.tail +: instructions.tail, cursor, acc)
          case 'D' if cursor == 'B'                                 =>
            loop(instructions.head.tail +: instructions.tail, 'D', acc)
          case 'D' if cursor == '6'                                 =>
            loop(instructions.head.tail +: instructions.tail, 'A', acc)
          case 'D' if cursor == '7'                                 =>
            loop(instructions.head.tail +: instructions.tail, 'B', acc)
          case 'D' if cursor == '8'                                 =>
            loop(instructions.head.tail +: instructions.tail, 'C', acc)
          case 'D' if cursor == '2'                                 =>
            loop(instructions.head.tail +: instructions.tail, '6', acc)
          case 'D' if cursor == '3'                                 =>
            loop(instructions.head.tail +: instructions.tail, '7', acc)
          case 'D' if cursor == '4'                                 =>
            loop(instructions.head.tail +: instructions.tail, '8', acc)
          case 'D' if cursor == '1'                                 =>
            loop(instructions.head.tail +: instructions.tail, '3', acc)
          case 'L' if Seq('1', '2', '5', 'A', 'D').contains(cursor) =>
            loop(instructions.head.tail +: instructions.tail, cursor, acc)
          case 'L' if cursor == '3'                                 =>
            loop(instructions.head.tail +: instructions.tail, '2', acc)
          case 'L' if cursor == '4'                                 =>
            loop(instructions.head.tail +: instructions.tail, '3', acc)
          case 'L' if cursor == '6'                                 =>
            loop(instructions.head.tail +: instructions.tail, '5', acc)
          case 'L' if cursor == '7'                                 =>
            loop(instructions.head.tail +: instructions.tail, '6', acc)
          case 'L' if cursor == '8'                                 =>
            loop(instructions.head.tail +: instructions.tail, '7', acc)
          case 'L' if cursor == '9'                                 =>
            loop(instructions.head.tail +: instructions.tail, '8', acc)
          case 'L' if cursor == 'B'                                 =>
            loop(instructions.head.tail +: instructions.tail, 'A', acc)
          case 'L' if cursor == 'C'                                 =>
            loop(instructions.head.tail +: instructions.tail, 'B', acc)
          case 'R' if Seq('1', '4', '9', 'C', 'D').contains(cursor) =>
            loop(instructions.head.tail +: instructions.tail, cursor, acc)
          case 'R' if cursor == '2'                                 =>
            loop(instructions.head.tail +: instructions.tail, '3', acc)
          case 'R' if cursor == '3'                                 =>
            loop(instructions.head.tail +: instructions.tail, '4', acc)
          case 'R' if cursor == '5'                                 =>
            loop(instructions.head.tail +: instructions.tail, '6', acc)
          case 'R' if cursor == '6'                                 =>
            loop(instructions.head.tail +: instructions.tail, '7', acc)
          case 'R' if cursor == '7'                                 =>
            loop(instructions.head.tail +: instructions.tail, '8', acc)
          case 'R' if cursor == '8'                                 =>
            loop(instructions.head.tail +: instructions.tail, '9', acc)
          case 'R' if cursor == 'A'                                 =>
            loop(instructions.head.tail +: instructions.tail, 'B', acc)
          case 'R' if cursor == 'B'                                 =>
            loop(instructions.head.tail +: instructions.tail, 'C', acc)
    loop(instructions, '5', "")

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day02input.txt")): source =>
      source.getLines().toSeq
end Day02
