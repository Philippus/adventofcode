package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day22:
  def calculateNextNumber(secretNumber: BigInt): BigInt =
    val step1 = ((secretNumber * 64) ^ secretNumber) % 16777216
    val step2 = ((step1 / 32) ^ step1)               % 16777216
    ((step2 * 2048) ^ step2) % 16777216

  def findNthSecretNumber(secretNumber: BigInt, n: Int): BigInt =
    @tailrec
    def loop(secretNumber: BigInt, i: Int): BigInt =
      if i == n then
        secretNumber
      else
        loop(calculateNextNumber(secretNumber), i + 1)

    loop(secretNumber, 0)

  def calculateSumOfNthSecretNumbers(secretNumbers: Seq[BigInt], n: Int): BigInt =
    secretNumbers.map(findNthSecretNumber(_, n)).sum

  def priceChangesToBananas(secretNumber: BigInt): Map[(Int, Int, Int, Int), Int] =
    @tailrec
    def loop(secretNumber: BigInt, i: Int, acc: Seq[Int]): Seq[Int] =
      if i == 1999 then
        acc
      else
        val nextNumber = calculateNextNumber(secretNumber)
        loop(nextNumber, i + 1, acc :+ nextNumber.toString.takeRight(1).toInt)

    val ends = loop(secretNumber, 0, Seq(secretNumber.toString.takeRight(1).toInt))
    ends.sliding(5).map: s =>
      ((s(1) - s.head, s(2) - s(1), s(3) - s(2), s(4) - s(3)), s(4))
    .distinctBy(_._1).toMap

  def mostBananas(secretNumbers: Seq[BigInt]): Int =
    val priceChangesToBananass = secretNumbers.map(priceChangesToBananas)
    val possiblePriceChanges   = priceChangesToBananass.flatMap(_.keys).distinct
    possiblePriceChanges.map: ps =>
      priceChangesToBananass.map:
        _.getOrElse(ps, 0)
      .sum
    .max

  def importLines(): Seq[BigInt] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq.map(s => BigInt(s.toInt))
end Day22
