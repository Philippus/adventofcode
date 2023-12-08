package adventofcode2015

object Day17:

  def getCombinations(containers: List[Int]): Seq[List[(Int, Int)]] =
    val indexedContainers = containers.zipWithIndex
    (for
      i <- 1.to(indexedContainers.length)
    yield indexedContainers.combinations(i).toSeq).flatten

  def countCombinations(containers: List[Int], eggnogToStore: Int): Int =
    val combos = getCombinations(containers)
    combos.count(combo => combo.map(_._1).sum == eggnogToStore)

  def findMinimumAmountOfContainers(containers: List[Int], eggnogToStore: Int): Int =
    val combos = getCombinations(containers)
    combos.filter(combo => combo.map(_._1).sum == eggnogToStore).minBy(_.length).length

  def countCombinationsWithContainerLimit(containers: List[Int], containerLimit: Int, eggnogToStore: Int): Int =
    containers.zipWithIndex.combinations(containerLimit).toSeq
      .count(combo => combo.map(_._1).sum == eggnogToStore)
end Day17
