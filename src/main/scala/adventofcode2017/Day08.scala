package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day08:
  @tailrec
  def followInstructions(instructions: Seq[String], registers: Map[String, Int], highestValueHeld: Int): (Int, Int) =
    if instructions.isEmpty then
      (registers.maxBy(_._2)._2, highestValueHeld)
    else
      instructions.head match
        case s"$reg1 $incDec $by if $reg2 $cond $value" =>
          cond match
            case ">" if registers.getOrElse(reg2, 0) > value.toInt   =>
              val currentVal = registers.getOrElse(reg1, 0)
              val nextVal    = if incDec == "inc" then currentVal + by.toInt else currentVal - by.toInt
              followInstructions(
                instructions.tail,
                registers.updated(reg1, nextVal),
                math.max(highestValueHeld, nextVal)
              )
            case "<" if registers.getOrElse(reg2, 0) < value.toInt   =>
              val currentVal = registers.getOrElse(reg1, 0)
              val nextVal    = if incDec == "inc" then currentVal + by.toInt else currentVal - by.toInt
              followInstructions(
                instructions.tail,
                registers.updated(reg1, nextVal),
                math.max(highestValueHeld, nextVal)
              )
            case ">=" if registers.getOrElse(reg2, 0) >= value.toInt =>
              val currentVal = registers.getOrElse(reg1, 0)
              val nextVal    = if incDec == "inc" then currentVal + by.toInt else currentVal - by.toInt
              followInstructions(
                instructions.tail,
                registers.updated(reg1, nextVal),
                math.max(highestValueHeld, nextVal)
              )
            case "<=" if registers.getOrElse(reg2, 0) <= value.toInt =>
              val currentVal = registers.getOrElse(reg1, 0)
              val nextVal    = if incDec == "inc" then currentVal + by.toInt else currentVal - by.toInt
              followInstructions(
                instructions.tail,
                registers.updated(reg1, nextVal),
                math.max(highestValueHeld, nextVal)
              )
            case "==" if registers.getOrElse(reg2, 0) == value.toInt =>
              val currentVal = registers.getOrElse(reg1, 0)
              val nextVal    = if incDec == "inc" then currentVal + by.toInt else currentVal - by.toInt
              followInstructions(
                instructions.tail,
                registers.updated(reg1, nextVal),
                math.max(highestValueHeld, nextVal)
              )
            case "!=" if registers.getOrElse(reg2, 0) != value.toInt =>
              val currentVal = registers.getOrElse(reg1, 0)
              val nextVal    = if incDec == "inc" then currentVal + by.toInt else currentVal - by.toInt
              followInstructions(
                instructions.tail,
                registers.updated(reg1, nextVal),
                math.max(highestValueHeld, nextVal)
              )
            case _                                                   =>
              followInstructions(instructions.tail, registers, highestValueHeld)

  def readInputFile(): Seq[String] =
    Using.resource(Source.fromResource("2017/day08input.txt")):
      _.getLines().toSeq
end Day08
