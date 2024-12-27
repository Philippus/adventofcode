package adventofcode2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day05:
  @tailrec
  def process(program: List[Int], iP: Int, input: Int, output: Int = -1): Int =
    f"${program(iP)}%05d" match
      case s"${modes}01" =>
        val param1 = if modes(2) == '0' then program(program(iP + 1)) else program(iP + 1)
        val param2 = if modes(1) == '0' then program(program(iP + 2)) else program(iP + 2)
        val param3 = program(iP + 3)
        process(
          program.updated(param3, param1 + param2),
          iP + 4,
          input,
          output
        )
      case s"${modes}02" =>
        val param1 = if modes(2) == '0' then program(program(iP + 1)) else program(iP + 1)
        val param2 = if modes(1) == '0' then program(program(iP + 2)) else program(iP + 2)
        val param3 = program(iP + 3)
        process(
          program.updated(param3, param1 * param2),
          iP + 4,
          input,
          output
        )
      case s"${modes}03" =>
        process(program.updated(program(iP + 1), input), iP + 2, input, output)
      case s"${modes}04" =>
        val param1 = if modes(2) == '0' then program(program(iP + 1)) else program(iP + 1)
        process(program, iP + 2, input, param1)
      case s"${modes}05" =>
        val param1 = if modes(2) == '0' then program(program(iP + 1)) else program(iP + 1)
        val param2 = if modes(1) == '0' then program(program(iP + 2)) else program(iP + 2)
        if param1 != 0 then
          process(program, param2, input, output)
        else
          process(program, iP + 3, input, output)
      case s"${modes}06" =>
        val param1 = if modes(2) == '0' then program(program(iP + 1)) else program(iP + 1)
        val param2 = if modes(1) == '0' then program(program(iP + 2)) else program(iP + 2)
        if param1 == 0 then
          process(program, param2, input, output)
        else
          process(program, iP + 3, input, output)
      case s"${modes}07" =>
        val param1 = if modes(2) == '0' then program(program(iP + 1)) else program(iP + 1)
        val param2 = if modes(1) == '0' then program(program(iP + 2)) else program(iP + 2)
        val param3 = program(iP + 3)
        if param1 < param2 then
          process(program.updated(param3, 1), iP + 4, input, output)
        else
          process(program.updated(param3, 0), iP + 4, input, output)
      case s"${modes}08" =>
        val param1 = if modes(2) == '0' then program(program(iP + 1)) else program(iP + 1)
        val param2 = if modes(1) == '0' then program(program(iP + 2)) else program(iP + 2)
        val param3 = program(iP + 3)
        if param1 == param2 then
          process(program.updated(param3, 1), iP + 4, input, output)
        else
          process(program.updated(param3, 0), iP + 4, input, output)
      case s"${modes}99" =>
        output

  def importLines(): List[Int] =
    Using.resource(Source.fromResource("2019/day05input.txt")):
      _.getLines().toList.head.split(',').toList.map(_.toInt)
end Day05
