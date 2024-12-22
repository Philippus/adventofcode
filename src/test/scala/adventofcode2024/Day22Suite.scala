package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day22.*
import munit.FunSuite

class Day22Suite extends FunSuite:
  test("calculates next secret number"):
    assertEquals(calculateNextNumber(BigInt(123)), BigInt(15887950))

  test("calculates nth secret number"):
    assertEquals(findNthSecretNumber(123, 10), BigInt(5908254))

  test("calculates sum of 2000th numbers for the sample"):
    assertEquals(calculateSumOfNthSecretNumbers(importSampleLines(), 2000), BigInt(37327623))

  test("calculates sum of 2000th numbers for the input"):
    assertEquals(calculateSumOfNthSecretNumbers(importLines(), 2000), BigInt(18941802053L))

  test("figures out best sequence to get most bananas for the sample"):
    assertEquals(mostBananas(Seq(BigInt(1), BigInt(2), BigInt(3), BigInt(2024))), 23)

  test("figures out best sequence to get most bananas for the input"):
    assertEquals(mostBananas(importLines()), 2218)

  def importSampleLines(): Seq[BigInt] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq.map(s => BigInt(s.toInt))
end Day22Suite
