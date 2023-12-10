package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day20:
  def presentsForHouse(house: Int): Int =
    val properDivisors = 1.to(house / 2).filter(i => house % i == 0) :+ house
    properDivisors.map(_.*(10)).sum

  def presentsForHousePartTwo(house: Int): Int =
    val properDivisors = 1.to(house / 2).filter(i => (i * 50) > house).filter(i => house % i == 0) :+ house
    properDivisors.map(_.*(11)).sum

  def findFirstHouseForNumberOfPresents(presents: Int): Int =
    var house = 1
    while (presentsForHouse(house) < presents)
      house += 1
    house

  def findFirstHouseForNumberOfPresentsPartTwo(presents: Int): Int =
    var house = 1
    while (presentsForHousePartTwo(house) < presents)
      house += 1
    house
end Day20
