package adventofcode2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day01:
  def fuelRequired(mass: Int): Int =
    ((mass.toDouble / 3.toDouble).floor - 2.0D).toInt

  def fuelRequiredIncludingFuel(mass: Int): Int =
    @tailrec
    def loop(mass: Int, acc: Int): Int =
      if (fuelRequired(mass) <= 0)
        acc
      else
        loop(fuelRequired(mass), acc + fuelRequired(mass))
    loop(mass, 0)

  def readInputFile(): Seq[Int] =
    Using.resource(Source.fromResource("2019/day01input.txt")):
      _.getLines().toSeq.map(_.toInt)
end Day01
