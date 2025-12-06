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

      val diffs        = combos.flatMap {
        case m1 :: m2 :: _ =>
          List(
            (m1.id, (m2.x - m1.x).sign, (m2.y - m1.y).sign, (m2.z - m1.z).sign),
            (m2.id, (m1.x - m2.x).sign, (m1.y - m2.y).sign, (m1.z - m2.z).sign)
          )
      }.toList
      val updatedMoons = moons.map: moon =>
        val diffX = diffs.filter(_._1 == moon.id).map(_._2).sum
        val diffY = diffs.filter(_._1 == moon.id).map(_._3).sum
        val diffZ = diffs.filter(_._1 == moon.id).map(_._4).sum
        moon.copy(
          x = moon.x + moon.velX + diffX,
          y = moon.y + moon.velY + diffY,
          z = moon.z + moon.velZ + diffZ,
          velX = moon.velX + diffX,
          velY = moon.velY + diffY,
          velZ = moon.velZ + diffZ
        )
      calculateEnergy(updatedMoons, cycles - 1)

  @tailrec
  def findCyclesOfMoons(
      moons: List[Moon],
      cycles: Int,
      initialMoons: List[Moon],
      currentMoons: List[Moon],
      found: (Int, Int, Int)
  ): BigInt = {
    if found._1 > 0 && found._2 > 0 && found._3 > 0 then
      lcm(lcm(BigInt(found._1), BigInt(found._2)), BigInt(found._3))
    else if found._1 == 0 && initialMoons.map(m => (m.x, m.velX)) == currentMoons.map(m => (m.x, m.velX)) && cycles > 0
    then
      findCyclesOfMoons(moons, cycles, initialMoons, currentMoons, found.copy(_1 = cycles))
    else if found._2 == 0 && initialMoons.map(m => (m.y, m.velY)) == currentMoons.map(m => (m.y, m.velY)) && cycles > 0
    then
      findCyclesOfMoons(moons, cycles, initialMoons, currentMoons, found.copy(_2 = cycles))
    else if found._3 == 0 && initialMoons.map(m => (m.z, m.velZ)) == currentMoons.map(m => (m.z, m.velZ)) && cycles > 0
    then
      findCyclesOfMoons(moons, cycles, initialMoons, currentMoons, found.copy(_3 = cycles))
    else
      val combos       = moons.combinations(2)
      val diffs        = combos.flatMap:
        case m1 :: m2 :: _ =>
          List(
            (m1.id, (m2.x - m1.x).sign, (m2.y - m1.y).sign, (m2.z - m1.z).sign),
            (m2.id, (m1.x - m2.x).sign, (m1.y - m2.y).sign, (m1.z - m2.z).sign)
          )
      .toList
      val updatedMoons = moons.map: moon =>
        val diffX = diffs.filter(_._1 == moon.id).map(_._2).sum
        val diffY = diffs.filter(_._1 == moon.id).map(_._3).sum
        val diffZ = diffs.filter(_._1 == moon.id).map(_._4).sum
        moon.copy(
          x = moon.x + moon.velX + diffX,
          y = moon.y + moon.velY + diffY,
          z = moon.z + moon.velZ + diffZ,
          velX = moon.velX + diffX,
          velY = moon.velY + diffY,
          velZ = moon.velZ + diffZ
        )
      findCyclesOfMoons(updatedMoons, cycles + 1, initialMoons, updatedMoons, found)
  }

  def lcm(a: BigInt, b: BigInt): BigInt =
    @tailrec
    def gdc(a: BigInt, b: BigInt): BigInt =
      if b == 0 then a else gdc(b, a % b)

    a * (b / gdc(a, b))

  def parse(input: String): List[Moon] =
    input.split('\n').zipWithIndex.map:
      case (s"<x=${x}, y=${y}, z=${z}>", id) => Moon(id, x.toInt, y.toInt, z.toInt, 0, 0, 0)
    .toList

  def importLines(): String =
    Using.resource(Source.fromResource("2019/day12input.txt")): source =>
      source.mkString
end Day12
