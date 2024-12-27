package adventofcode2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09:
  def param(program: Map[Long, Long], mode: Char, offset: Long, relativeBase: Long): Long =
    val value = program.getOrElse(offset, 0L)
    mode match
      case '0' => program.getOrElse(value, 0L)                // position mode
      case '1' => value                                       // immediate mode
      case '2' => program.getOrElse(relativeBase + value, 0L) // relative mode

  def writeParam(program: Map[Long, Long], mode: Char, offset: Long, relativeBase: Long): Long =
    val value = program.getOrElse(offset, 0L)
    mode match
      case '0' => value
      case '1' => value
      case '2' => value + relativeBase

  @tailrec
  def process(
      program: Map[Long, Long],
      iP: Long,
      input: Seq[Long],
      output: Seq[Long] = Seq.empty[Long],
      relativeBase: Long = 0L
  ): Seq[Long] =
    f"${program(iP)}%05d" match
      case s"${modes}01" =>
        val param1 = param(program, modes(2), iP + 1L, relativeBase)
        val param2 = param(program, modes(1), iP + 2L, relativeBase)
        val param3 = writeParam(program, modes(0), iP + 3L, relativeBase)
        process(program.updated(param3, param1 + param2), iP + 4L, input, output, relativeBase)
      case s"${modes}02" =>
        val param1 = param(program, modes(2), iP + 1L, relativeBase)
        val param2 = param(program, modes(1), iP + 2L, relativeBase)
        val param3 = writeParam(program, modes(0), iP + 3L, relativeBase)
        process(program.updated(param3, param1 * param2), iP + 4L, input, output, relativeBase)
      case s"${modes}03" =>
        val param1 = writeParam(program, modes(2), iP + 1L, relativeBase)
        process(program.updated(param1, input.head), iP + 2L, input.tail, output, relativeBase)
      case s"${modes}04" =>
        val param1 = param(program, modes(2), iP + 1L, relativeBase)
        process(program, iP + 2L, input, output :+ param1, relativeBase)
      case s"${modes}05" =>
        val param1 = param(program, modes(2), iP + 1L, relativeBase)
        val param2 = param(program, modes(1), iP + 2L, relativeBase)
        if param1 != 0L then
          process(program, param2, input, output, relativeBase)
        else
          process(program, iP + 3L, input, output, relativeBase)
      case s"${modes}06" =>
        val param1 = param(program, modes(2), iP + 1L, relativeBase)
        val param2 = param(program, modes(1), iP + 2L, relativeBase)
        if param1 == 0 then
          process(program, param2, input, output, relativeBase)
        else
          process(program, iP + 3L, input, output, relativeBase)
      case s"${modes}07" =>
        val param1 = param(program, modes(2), iP + 1L, relativeBase)
        val param2 = param(program, modes(1), iP + 2L, relativeBase)
        val param3 = writeParam(program, modes(0), iP + 3L, relativeBase)
        if param1 < param2 then
          process(program.updated(param3, 1L), iP + 4L, input, output, relativeBase)
        else
          process(program.updated(param3, 0L), iP + 4L, input, output, relativeBase)
      case s"${modes}08" =>
        val param1 = param(program, modes(2), iP + 1L, relativeBase)
        val param2 = param(program, modes(1), iP + 2L, relativeBase)
        val param3 = writeParam(program, modes(0), iP + 3L, relativeBase)
        if param1 == param2 then
          process(program.updated(param3, 1L), iP + 4L, input, output, relativeBase)
        else
          process(program.updated(param3, 0L), iP + 4L, input, output, relativeBase)
      case s"${modes}09" =>
        val param1 = param(program, modes(2), iP + 1L, relativeBase)
        process(program, iP + 2L, input, output, relativeBase + param1)
      case s"${modes}99" =>
        output

  def importLines(): Map[Long, Long] =
    Using.resource(Source.fromResource("2019/day09input.txt")): source =>
      source.getLines().toList.head.split(',').zipWithIndex.map: (a, i) =>
        i.toLong -> a.toLong
      .toMap
end Day09
