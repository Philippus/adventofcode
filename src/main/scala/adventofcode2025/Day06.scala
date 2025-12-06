package adventofcode2025

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06:
  def grandTotal(lines: List[String]): Long =
    val termsOrFactorss = lines.init.map: line =>
      line.split("  *").filter(_.nonEmpty).map(_.toLong).toList
    val ops             = lines.last.split("  *").toList
    val subTotals       =
      for
        i <- ops.indices
      yield ops(i) match
        case "+" =>
          termsOrFactorss.map(line => line(i)).sum
        case "*" =>
          termsOrFactorss.map(line => line(i)).product
    subTotals.sum

  def rightToLeftGrandTotal(lines: List[String]): Long =
    @tailrec
    def loop(idx: Int, termsOrFactors: List[Long], total: Long): Long =
      if idx == -1 then
        total
      else
        val numOpt = lines.init.flatMap: line =>
          line.lift(idx)
        .mkString.filter(_.isDigit).toLongOption
        (numOpt, lines.last.lift(idx)) match
          case (Some(term), Some('+'))   =>
            loop(idx - 1, List.empty[Long], total + (termsOrFactors :+ term).sum)
          case (Some(factor), Some('*')) =>
            loop(idx - 1, List.empty[Long], total + (termsOrFactors :+ factor).product)
          case (Some(termOrFactor), _)   =>
            loop(idx - 1, termsOrFactors :+ termOrFactor, total)
          case (None, _)                 =>
            loop(idx - 1, termsOrFactors, total)

    loop(lines.maxBy(_.length).length - 1, List.empty[Long], 0L)

  def importLines(): String =
    Using.resource(Source.fromResource("2025/day06input.txt")): source =>
      source.mkString
end Day06
