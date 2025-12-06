package adventofcode2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12:
  case class Moon(id: Int, x: Int, y: Int, z: Int, velX: Int, velY: Int, velZ: Int):
    def energy: Int = (x.abs + y.abs + z.abs) * (velX.abs + velY.abs + velZ.abs)

  @tailrec
  def calculateEnergy(moons: List[Moon], cycles: Int): Int =
    if cycles == 0 then
      moons.map(_.energy).sum
    else
      val combos = moons.combinations(2)

      val diffs = combos.flatMap {
        case m1 :: m2 :: _ =>
          List((m1.id, (m2.x - m1.x).sign, (m2.y - m1.y).sign, (m2.z - m1.z).sign), (m2.id, (m1.x - m2.x).sign, (m1.y - m2.y).sign, (m1.z - m2.z).sign))
      }.toList
      val updatedMoons = moons.map: moon =>
        val diffX = diffs.filter(_._1 == moon.id).map(_._2).sum
        val diffY = diffs.filter(_._1 == moon.id).map(_._3).sum
        val diffZ = diffs.filter(_._1 == moon.id).map(_._4).sum
        val newMoon = moon.copy(velX = moon.velX + diffX, velY = moon.velY + diffY, velZ = moon.velZ + diffZ)
        newMoon.copy(x = newMoon.x + newMoon.velX, y = newMoon.y + newMoon.velY, z = newMoon.z + newMoon.velZ)
      calculateEnergy(updatedMoons, cycles - 1)

  def parse(input: String): List[Moon] =
    input.split('\n').zipWithIndex.map:
      case (s"<x=${x}, y=${y}, z=${z}>", id) => Moon(id, x.toInt, y.toInt, z.toInt, 0, 0, 0)
    .toList

  def importLines(): String =
    Using.resource(Source.fromResource("2019/day12input.txt")): source =>
      source.mkString
end Day12
