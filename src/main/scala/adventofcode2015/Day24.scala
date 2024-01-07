package adventofcode2015

import scala.io.Source
import scala.util.Using

object Day24:
  def determineMinimumCombinations(packages: Seq[BigInt]): BigInt =
    val weight = packages.sum / 3
    (for
      combination     <-
        packages.combinations(14).filter(_.sum == weight) // determined that the biggest group contains 14 packages
      j               <- 1 to 14
      combinationTwo  <- packages.diff(combination).combinations(j).filter(_.sum == weight)
      combinationThree =
        packages.diff(combination).diff(combinationTwo) // last group's weight doesn't need to be checked
    yield Seq(combination, combinationTwo, combinationThree).minBy(_.length)).minBy(x =>
      (x.length, x.product)
    ).product

  def determineMinimumCombinationsForPartTwo(packages: Seq[BigInt]): BigInt =
    val weight = packages.sum / 4
    (for
      combination      <-
        packages.combinations(13).filter(_.sum == weight) // determined that the biggest group contains 13 packages
      j                <- 1 to 13
      combinationTwo   <- packages.diff(combination).combinations(j).filter(_.sum == weight)
      k                <- 1 to 13
      combinationThree <- packages.diff(combination).diff(combinationTwo).combinations(k).filter(_.sum == weight)
      combinationFour   =
        packages.diff(combination).diff(combinationTwo).diff(
          combinationThree
        ) // last group's weight doesn't need to be checked
    yield Seq(combination, combinationTwo, combinationThree, combinationFour).minBy(_.length)).minBy(x =>
      (x.length, x.product)
    ).product

  def importLines(): Seq[BigInt] =
    Using.resource(Source.fromResource("2015/day24input.txt")): source =>
      source.getLines().toSeq.map(_.toInt)
end Day24
