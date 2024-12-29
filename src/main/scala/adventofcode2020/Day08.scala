package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day08:
  def runBootCode(code: Seq[String]): Int =
    @tailrec
    def loop(iP: Int, ran: Seq[Int], acc: Int): Int =
      if ran.contains(iP) then
        acc
      else
        code(iP) match
          case s"nop +$i" =>
            loop(iP + 1, ran :+ iP, acc)
          case s"nop -$i" =>
            loop(iP + 1, ran :+ iP, acc)
          case s"acc +$i" =>
            loop(iP + 1, ran :+ iP, acc + i.toInt)
          case s"acc -$i" =>
            loop(iP + 1, ran :+ iP, acc - i.toInt)
          case s"jmp +$i" =>
            loop(iP + i.toInt, ran :+ iP, acc)
          case s"jmp -$i" =>
            loop(iP - i.toInt, ran :+ iP, acc)

    loop(0, Seq.empty[Int], 0)

  def transformCode(line: String): String =
    line match
      case s"nop $i" => s"jmp $i"
      case s"jmp $i" => s"nop $i"

  def fixBootCode(code: Seq[String]): Int =
    @tailrec
    def loop(code: Seq[String], iP: Int, ran: Map[Int, Int], acc: Int): Option[Int] =
      if iP >= code.length then
        Some(acc)
      else if ran.getOrElse(iP, 0) == 5 then
        None
      else
        code(iP) match
          case s"nop +$i" =>
            loop(code, iP + 1, ran.updated(iP, ran.getOrElse(iP, 0) + 1), acc)
          case s"nop -$i" =>
            loop(code, iP + 1, ran.updated(iP, ran.getOrElse(iP, 0) + 1), acc)
          case s"acc +$i" =>
            loop(code, iP + 1, ran.updated(iP, ran.getOrElse(iP, 0) + 1), acc + i.toInt)
          case s"acc -$i" =>
            loop(code, iP + 1, ran.updated(iP, ran.getOrElse(iP, 0) + 1), acc - i.toInt)
          case s"jmp +$i" =>
            loop(code, iP + i.toInt, ran.updated(iP, ran.getOrElse(iP, 0) + 1), acc)
          case s"jmp -$i" =>
            loop(code, iP - i.toInt, ran.updated(iP, ran.getOrElse(iP, 0) + 1), acc)
    (for
      i      <- code.indices
      if !code(i).contains("acc")
      newCode = code.updated(i, transformCode(code(i)))
    yield loop(newCode, 0, Map.empty[Int, Int], 0)).find(_.nonEmpty).get.get

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day08
