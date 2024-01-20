package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10:
  def findBot(instructions: Seq[String], bots: Map[Int, Set[Int]], setToBeFound: Set[Int]): Int =
    def loop(instructions: Seq[String], bots: Map[Int, Set[Int]]): Int =
      if instructions.isEmpty then
        -1
      else
        val instruction = instructions.head
        instruction match
          case s"bot $x gives low to output $y and high to output $z" =>
            if (bots.getOrElse(x.toInt, Set.empty).size != 2)
              loop(instructions.tail :+ instruction, bots)
            else if (bots(x.toInt) == setToBeFound)
              x.toInt
            else
              loop(
                instructions.tail,
                bots ++ Map(
                  x.toInt -> Set.empty
                )
              )
          case s"bot $x gives low to output $y and high to bot $z"    =>
            if (bots.getOrElse(x.toInt, Set.empty).size != 2)
              loop(instructions.tail :+ instruction, bots)
            else if (bots(x.toInt) == setToBeFound)
              x.toInt
            else
              loop(
                instructions.tail,
                bots ++ Map(
                  z.toInt -> (bots.getOrElse(z.toInt, Set.empty) + bots(x.toInt).max),
                  x.toInt -> Set.empty
                )
              )
          case s"bot $x gives low to bot $y and high to output $z"    =>
            if (bots.getOrElse(x.toInt, Set.empty).size != 2)
              loop(instructions.tail :+ instruction, bots)
            else if (bots(x.toInt) == setToBeFound)
              x.toInt
            else
              loop(
                instructions.tail,
                bots ++ Map(
                  y.toInt -> (bots.getOrElse(y.toInt, Set.empty) + bots(x.toInt).min),
                  x.toInt -> Set.empty
                )
              )
          case s"bot $x gives low to bot $y and high to bot $z"       =>
            if (bots.getOrElse(x.toInt, Set.empty).size != 2)
              loop(instructions.tail :+ instruction, bots)
            else if (bots(x.toInt) == setToBeFound)
              x.toInt
            else
              loop(
                instructions.tail,
                bots ++ Map(
                  y.toInt -> (bots.getOrElse(y.toInt, Set.empty) + bots(x.toInt).min),
                  z.toInt -> (bots.getOrElse(z.toInt, Set.empty) + bots(x.toInt).max),
                  x.toInt -> Set.empty
                )
              )
          case _                                                      =>
            loop(instructions.tail, bots)

    loop(instructions, bots)

  def getOutputs(instructions: Seq[String], bots: Map[Int, Set[Int]]): Map[Int, Int] =
    @tailrec
    def loop(instructions: Seq[String], bots: Map[Int, Set[Int]], outputs: Map[Int, Int]): Map[Int, Int] =
      if instructions.isEmpty then
        outputs
      else
        val instruction = instructions.head
        instruction match
          case s"bot $x gives low to output $y and high to output $z" =>
            if (bots.getOrElse(x.toInt, Set.empty).size != 2)
              loop(instructions.tail :+ instruction, bots, outputs)
            else
              loop(
                instructions.tail,
                bots ++ Map(
                  x.toInt -> Set.empty
                ),
                outputs ++ Map(
                  y.toInt -> bots(x.toInt).min,
                  z.toInt -> bots(x.toInt).max
                )
              )
          case s"bot $x gives low to output $y and high to bot $z"    =>
            if (bots.getOrElse(x.toInt, Set.empty).size != 2)
              loop(instructions.tail :+ instruction, bots, outputs)
            else
              loop(
                instructions.tail,
                bots ++ Map(
                  z.toInt -> (bots.getOrElse(z.toInt, Set.empty) + bots(x.toInt).max),
                  x.toInt -> Set.empty
                ),
                outputs + (y.toInt -> bots(x.toInt).min)
              )
          case s"bot $x gives low to bot $y and high to output $z"    =>
            if (bots.getOrElse(x.toInt, Set.empty).size != 2)
              loop(instructions.tail :+ instruction, bots, outputs)
            else
              loop(
                instructions.tail,
                bots ++ Map(
                  y.toInt -> (bots.getOrElse(y.toInt, Set.empty) + bots(x.toInt).min),
                  x.toInt -> Set.empty
                ),
                outputs + (z.toInt -> bots(x.toInt).max)
              )
          case s"bot $x gives low to bot $y and high to bot $z"       =>
            if (bots.getOrElse(x.toInt, Set.empty).size != 2)
              loop(instructions.tail :+ instruction, bots, outputs)
            else
              loop(
                instructions.tail,
                bots ++ Map(
                  y.toInt -> (bots.getOrElse(y.toInt, Set.empty) + bots(x.toInt).min),
                  z.toInt -> (bots.getOrElse(z.toInt, Set.empty) + bots(x.toInt).max),
                  x.toInt -> Set.empty
                ),
                outputs
              )
          case _                                                      =>
            loop(instructions.tail, bots, outputs)

    loop(instructions, bots, Map.empty)

  def initializeBots(instructions: Seq[String]): Map[Int, Set[Int]] =
    @tailrec
    def loop(instructions: Seq[String], bots: Map[Int, Set[Int]]): Map[Int, Set[Int]] =
      if instructions.isEmpty then
        bots
      else
        val instruction = instructions.head
        instruction match
          case s"value $x goes to bot $y" =>
            loop(instructions.tail, bots + (y.toInt -> (bots.getOrElse(y.toInt, Set.empty) + x.toInt)))
          case _                          =>
            loop(instructions.tail, bots)

    loop(instructions, Map.empty)

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day10input.txt")): source =>
      source.getLines().toSeq
end Day10
