package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day07:
  case class Bag(color: String, contents: List[Bag])

  def findShinyGoldBagHoldingBags(bags: List[Bag]): Long =
    @tailrec
    def loop(bagsToFind: List[Bag], acc: List[Bag]): List[Bag] =
      if bagsToFind.isEmpty
      then acc
      else
        val foundBags =
          for
            bagToFind <- bagsToFind
            bag       <- bags
            if bag.contents.map(_.color).contains(bagToFind.color)
          yield bag
        loop(foundBags, acc ++ foundBags)

    loop(List(Bag("shiny gold", List.empty[Bag])), List.empty[Bag]).distinct.length

  def countBagsInsideShinyGoldBag(bags: List[Bag]): Long =
    @tailrec
    def loop(contentsToFindOfBags: List[Bag], acc: List[Bag]): List[Bag] =
      if contentsToFindOfBags.isEmpty
      then acc
      else
        val foundBags =
          for
            contentsToFindOfBag <- contentsToFindOfBags
            content             <- contentsToFindOfBag.contents
            bag                 <- bags
            if content.color == bag.color
          yield bags.find(_.color == bag.color).get
        loop(foundBags, acc ++ contentsToFindOfBags.flatMap(_.contents))

    loop(List(bags.find(_.color == "shiny gold").get), List.empty[Bag]).length

  def parse(input: String): List[Bag] =
    input.split("\n").toList.map:
      case s"$color bags contain no other bags." =>
        Bag(color, List.empty[Bag])
      case s"$color bags contain $otherBags."    =>
        val contents = otherBags.split(", ").toList.map:
          case s"1 $subColor bag"   =>
            List(Bag(subColor, List.empty[Bag]))
          case s"$n $subColor bags" =>
            List.fill(n.toInt)(Bag(subColor, List.empty[Bag]))
        Bag(color, contents.flatten)

  def importLines(): String =
    Using.resource(Source.fromResource("2020/day07input.txt")): source =>
      source.mkString
end Day07
