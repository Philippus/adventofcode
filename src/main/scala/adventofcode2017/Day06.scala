package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06:
  @tailrec
  def redistributeBlocks(
      banks: Seq[Int],
      seenConfigurations: Seq[Seq[Int]],
      cycles: Int,
      partTwo: Boolean = false
  ): Int =
    val banksAsArray         = banks.toArray
    val bankToRedistribute   = banksAsArray.indexOf(banksAsArray.max)
    var blocksToRedistribute = banksAsArray.max
    banksAsArray(bankToRedistribute) = 0
    var bankToRedistributeTo = (bankToRedistribute + 1) % banks.length
    while blocksToRedistribute > 0 do
      banksAsArray(bankToRedistributeTo) = banksAsArray(bankToRedistributeTo) + 1
      blocksToRedistribute -= 1
      bankToRedistributeTo = (bankToRedistributeTo + 1) % banks.length
    val newBanks             = banksAsArray.toSeq
    if seenConfigurations.contains(newBanks) then
      if partTwo then
        seenConfigurations.length - seenConfigurations.indexOf(newBanks) + 1
      else
        cycles + 1
    else
      redistributeBlocks(newBanks, seenConfigurations :+ banks, cycles + 1, partTwo)

  def readInputFile: Seq[Int] =
    Using.resource(Source.fromResource("2017/day06input.txt")):
      _.getLines().toSeq.head.split("\t").toSeq.map(_.toInt)
end Day06
