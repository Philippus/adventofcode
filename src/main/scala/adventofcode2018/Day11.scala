package adventofcode2018

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day11:
  def powerLevelOfFuelCell(x: Int, y: Int, serialNumber: Int): Int =
    val rackID = x + 10
    (((rackID * y + serialNumber) * rackID / 100) % 10) - 5

  def gridWithLargestPower(serialNumber: Int): ((Int, Int), Int) =
    (for
      x <- 1 to 298
      y <- 1 to 298
    yield (
      (x, y),
      powerLevelOfFuelCell(x, y, serialNumber) +
        powerLevelOfFuelCell(x + 1, y, serialNumber) +
        powerLevelOfFuelCell(x + 2, y, serialNumber) +
        powerLevelOfFuelCell(x, y + 1, serialNumber) +
        powerLevelOfFuelCell(x + 1, y + 1, serialNumber) +
        powerLevelOfFuelCell(x + 2, y + 1, serialNumber) +
        powerLevelOfFuelCell(x, y + 2, serialNumber) +
        powerLevelOfFuelCell(x + 1, y + 2, serialNumber) +
        powerLevelOfFuelCell(x + 2, y + 2, serialNumber)
    ))
      .maxBy(_._2)

  def powerLevelOfFuelCellWithSize(x: Int, y: Int, serialNumber: Int, size: Int): Int =
    (for
      x <- x to x + size
      y <- y to y + size
      if x <= 300 && y <= 300
    yield powerLevelOfFuelCell(x, y, serialNumber)).sum

  def gridWithLargestPowerWithSize(serialNumber: Int): ((Int, Int, Int), Int) =
    val map: mutable.Map[(Int, Int, Int, Int), Int] = mutable.Map[(Int, Int, Int, Int), Int]()
    (for
      size <- 1 to 300
      x    <- 1 to (300 - size + 1)
      y    <- 1 to (300 - size + 1)
    yield (
      (x, y, size), {
        val powerLevel =
          if size == 1 then // bootstrap the map
            powerLevelOfFuelCell(x, y, serialNumber)
          else if size % 2 == 0 then // if size is even we can take the sum of the 4 quadrants to have 4 map lookups,
            // i.e. sum up all the x's like this:
            // xxxx   xxzz   zzxx   zzzz   zzzz
            // xxxx = xxzz + zzxx + zzzz + zzzz
            // xxxx   zzzz   zzzz   xxzz   zzxx
            // xxxx   zzzz   zzzz   xxzz   zzxx
            map(x, y, serialNumber, size / 2) +
              map(x + size / 2, y, serialNumber, size / 2) +
              map(x, y + size / 2, serialNumber, size / 2) +
              map(x + size / 2, y + size / 2, serialNumber, size / 2)
          else // if odd we can do this with 5 lookups, i.e. sum the x's like this:
            // xxx   xxz   zxx   zzz   zzx   zzz
            // xxx = xxz + zxx - zxz + zzz + zzz
            // xxx   zzz   zxx   zzz   zzz   xzz
            map(x, y, serialNumber, size - 1) +
              map(x + 1, y + 1, serialNumber, size - 1) -
              map(x + 1, y + 1, serialNumber, size - 2) +
              map(x, y + size - 1, serialNumber, 1) +
              map(x + size - 1, y, serialNumber, 1)
        map.update((x, y, serialNumber, size), powerLevel)
        powerLevel
      }
    ))
      .maxBy(_._2)

  def importLines(): Int =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().map(_.toInt).next()
end Day11
