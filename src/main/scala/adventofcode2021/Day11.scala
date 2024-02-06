package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11:
  def determineNewFlashes(energyLevels: Array[Array[Int]]): (Int, Array[Array[Int]]) =
    var done                  = false
    var skip: Seq[(Int, Int)] = Seq.empty
    var flashes               = 0
    while !done do
      val tens     = (for
        x <- energyLevels.head.indices
        y <- energyLevels.indices
        if energyLevels(y)(x) > 9
      yield (x, y)).filterNot(skip.contains(_))
      flashes += tens.length
      val toUpdate = tens.flatMap(ten =>
        Seq(
          (ten._1 - 1, ten._2 - 1),
          (ten._1 - 1, ten._2),
          (ten._1 - 1, ten._2 + 1),
          (ten._1 + 1, ten._2 - 1),
          (ten._1 + 1, ten._2),
          (ten._1 + 1, ten._2 + 1),
          (ten._1, ten._2 - 1),
          (ten._1, ten._2 + 1)
        )
      ).filterNot(tens.contains(_))
      toUpdate.foreach(update =>
        if update._2 >= 0 && update._2 <= energyLevels.length - 1 && update._1 >= 0 && update._1 <= energyLevels.head.length - 1
        then
          energyLevels(update._2)(update._1) += 1
      )
      skip = skip ++ tens
      if tens.isEmpty then done = true
    (flashes, energyLevels)

  def flashes(energyLevels: Vector[Vector[Int]], initialSteps: Int, partTwo: Boolean = false): Int =
    @tailrec
    def loop(energyLevels: Array[Array[Int]], stepsLeft: Int, flashes: Int): Int =
      if !partTwo && stepsLeft == 0 then
        flashes
      else
        for
          x <- energyLevels.head.indices
          y <- energyLevels.indices
        yield energyLevels(y)(x) += 1
        val (newFlashes, newEnergyLevels) = determineNewFlashes(energyLevels)
        for
          x <- newEnergyLevels.head.indices
          y <- newEnergyLevels.indices
          if newEnergyLevels(y)(x) >= 10
        yield newEnergyLevels(y)(x) = 0
        if partTwo && newFlashes == energyLevels.length * energyLevels.head.length then
          initialSteps - stepsLeft + 1
        else
          loop(newEnergyLevels, stepsLeft - 1, flashes + newFlashes)

    loop(energyLevels.toArray.map(_.toArray), initialSteps, 0)

  def handleLines(s: Seq[String]): Vector[Vector[Int]] =
    Vector(s.map(_.map(_.asDigit).toVector): _*)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2021/day11input.txt")): source =>
      source.getLines().toSeq
end Day11
