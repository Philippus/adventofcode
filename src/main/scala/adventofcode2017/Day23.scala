package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day23:
  def followInstructions(instructions: Seq[String]): Long =
    @tailrec
    def loop(currentPos: Long, registers: Map[String, Long], acc: Long): Long =
      if currentPos >= instructions.length then
        acc
      else
        instructions(currentPos.toInt) match
          case s"set $x $y" if y.toLongOption.nonEmpty =>
            loop(currentPos + 1, registers + (x -> y.toLong), acc)
          case s"set $x $y"                            =>
            loop(currentPos + 1, registers + (x -> registers(y)), acc)
          case s"sub $x $y" if y.toLongOption.nonEmpty =>
            loop(currentPos + 1, registers + (x -> (registers(x) - y.toLong)), acc)
          case s"sub $x $y"                            =>
            loop(currentPos + 1, registers + (x -> (registers(x) - registers(y))), acc)
          case s"mul $x $y" if y.toLongOption.nonEmpty =>
            loop(currentPos + 1, registers + (x -> registers(x) * y.toLong), acc + 1)
          case s"mul $x $y"                            =>
            loop(currentPos + 1, registers + (x -> registers(x) * registers(y)), acc + 1)
          case s"jnz $x $y" if x.toLongOption.isEmpty  =>
            if registers(x) != 0L then
              loop(currentPos + y.toLong, registers, acc)
            else
              loop(currentPos + 1, registers, acc)
          case s"jnz $x $y"                            =>
            if x.toLong != 0L then
              loop(currentPos + y.toLong, registers, acc)
            else
              loop(currentPos + 1, registers, acc)

    val registers = Map(
      "a" -> 0L,
      "b" -> 0L,
      "c" -> 0L,
      "d" -> 0L,
      "e" -> 0L,
      "f" -> 0L,
      "g" -> 0L,
      "h" -> 0L
    )
    loop(0L, registers, 0L)

  def followPatchedInstructions(instructions: Seq[String]): Long =
    val patch               = Seq(
      "set g b",
      "mod g d",
      "jnz g 2",  // g = b % d
      "set f 0",
      "jnz f 2",
      "jnz 1 9",  // once f is 0 there is nothing left to do here
      "sub d -1",
      "set g d",
      "sub g b",
      "jnz g -9", // g = d - b
      "jnz 1 4",  // jump over padding
      "set a a",
      "set a a",
      "set a a"
    ) // padding so jnz 1 -23 still works
    val patchedInstructions = instructions.take(10) ++ patch ++ instructions.drop(24)

    @tailrec
    def loop(currentPos: Long, registers: Map[String, Long]): Long =
      if currentPos >= patchedInstructions.length then
        registers("h")
      else
        patchedInstructions(currentPos.toInt) match
          case s"mod $x $y"                            =>
            loop(currentPos + 1, registers + (x -> registers(x) % registers(y)))
          case s"set $x $y" if y.toLongOption.nonEmpty =>
            loop(currentPos + 1, registers + (x -> y.toLong))
          case s"set $x $y"                            =>
            loop(currentPos + 1, registers + (x -> registers(y)))
          case s"sub $x $y" if y.toLongOption.nonEmpty =>
            loop(currentPos + 1, registers + (x -> (registers(x) - y.toLong)))
          case s"sub $x $y"                            =>
            loop(currentPos + 1, registers + (x -> (registers(x) - registers(y))))
          case s"mul $x $y" if y.toLongOption.nonEmpty =>
            loop(currentPos + 1, registers + (x -> registers(x) * y.toLong))
          case s"mul $x $y"                            =>
            loop(currentPos + 1, registers + (x -> registers(x) * registers(y)))
          case s"jnz $x $y" if x.toLongOption.isEmpty  =>
            if registers(x) != 0L then
              loop(currentPos + y.toLong, registers)
            else
              loop(currentPos + 1, registers)
          case s"jnz $x $y"                            =>
            if x.toLong != 0L then
              loop(currentPos + y.toLong, registers)
            else
              loop(currentPos + 1, registers)

    val registers = Map(
      "a" -> 1L,
      "b" -> 0L,
      "c" -> 0L,
      "d" -> 0L,
      "e" -> 0L,
      "f" -> 0L,
      "g" -> 0L,
      "h" -> 0L
    )
    loop(0L, registers)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2017/Day23input.txt")):
      _.getLines().toSeq
end Day23
