package adventofcode2015

import scala.io.Source
import scala.util.Using

object Day24:
  def determineWeightOfGroup(packages: Seq[BigInt]): BigInt =
    packages.sum / 3

  def determineMinimumCombinations(packages: Seq[BigInt]): BigInt =
    val weight = packages.sum / 4
    (for
      //      i <- 8 to 14
      combination <- packages.combinations(13).filter(_.sum == weight)
      //      if combination.sum == weight
      j <- 1 to 13
      combinationTwo <- packages.diff(combination).combinations(j).filter(_.sum == weight)
      k <- 1 to 13
      combinationThree <- packages.diff(combination).diff(combinationTwo).combinations(j).filter(_.sum == weight)
      combinationFour = packages.diff(combination).diff(combinationTwo).diff(combinationThree)
    yield Seq(combination, combinationTwo, combinationThree, combinationFour).minBy(_.length)).minBy(x => (x.length, x.product)).product

//  def determineMinimumCombinations(packages: Seq[BigInt]): BigInt =
//    val weight = determineWeightOfGroup(packages)
//    (for
////      i <- 8 to 14
//      combination <- packages.combinations(14).filter(_.sum == weight)
////      if combination.sum == weight
//      j <- 8 to 14
//      combinationTwo <- packages.diff(combination).combinations(j).filter(_.sum == weight)
//      combinationThree = packages.diff(combination).diff(combinationTwo)
//    yield Seq(combination, combinationTwo, combinationThree).minBy(_.length)).minBy(x => (x.length, x.product)).product
//
//    while (!found)
//      val combinations = packages.combinations(i)
//      if combinations.exists(_.sum == weight) then found = true
//      i = i - 1
//    i

//  def checkPermutation(packages: Seq[Int]): Boolean =
//    val weightOfGroup = determineWeightOfGroup(packages)
//    var i = 0
//    var sum = 0
//    var found = false
//    while (!found)
//      sum = determineWeightOfGroup(packages.take(i))
//      if sum == weightOfGroup then found = true

//  def determineGroups(packages: Seq[Int]): Int =
//    val weight = determineWeightOfGroup(packages)
////    val perms = packages.permutations
//    val permutations = packages.permutations
////      .filter(_.scanLeft(0)(_ + _).intersect(Seq(weight, weight*2, weight *3)).size == 3)
////      .filter(_.scanLeft(0)(_ + _).contains(weight))
////    val permutations = Seq(Seq(10, 9, 1, 11, 7, 2, 8, 5, 4, 3), Seq(11, 9, 10, 8, 2, 7, 5, 4, 3, 1))
////    println(permutations.toSeq)
////    val groupCandidates = for
////      permutation <- permutations
////      scanLeft = permutation.scanLeft(0)(_ + _)
////      if scanLeft.contains(weight)
////      if scanLeft.contains(weight * 2)
////      if scanLeft.contains(weight * 3)
//////      permutation.ziip(scanLeft)
//////      if scanLeft.contains(weight * 2)
//////      if scanLeft.contains(weight * 3)
//////      if permutation.reverse.scanLeft(0)(_ + _).contains(weight)
////      i <- 7.to(packages.length - 14)
//////      j <- 7.to(packages.length - i - 7)
////      (groupOne, rest) = permutation.splitAt(i)
////      if groupOne.sum == weight
//////      if rest.splitAt(j)._1.sum == weight
//////      if rest.splitAt(j)._2.sum == weight
////    yield groupOne
//////    println(groupCandidates.toSeq)
//////    println(groupCandidates.map(_._1).contains(Seq(10, 9, 1)))
//////    val ones = groupCandidates.map(_._1).map(_.product)
//////    ones.sorted.headOption.toSeq
////      groupCandidates.minBy(x => (x.length, x.product)).product
////    groupCandidates.map(_._1).sortBy(x => x.product
//    val groupCandidates = for
//      permutation <- permutations
//      scanLeft = permutation.scanLeft(0)(_ + _)
//      if scanLeft.contains(weight)
//      if scanLeft.contains(weight * 2)
////      if scanLeft.contains(weight * 3)
//      indexOf = scanLeft.indexOf(weight)
//      groupOne = permutation.take(indexOf)
//    yield groupOne
//    groupCandidates.minBy(x => (x.length, x.product)).product
  def importLines(): Seq[BigInt] =
    Using.resource(Source.fromResource("2015/day24input.txt")): source =>
      source.getLines().toSeq.map(_.toInt)
end Day24
