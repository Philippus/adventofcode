package adventofcode2015

import scala.io.Source
import scala.util.Using

object Day16:
  case class Sue(number: String, things: Map[String, Int])

  def handleLine(line: String): Sue =
    line match
      case s"Sue $number: $thingOne: $i, $thingTwo: $j, $thingThree: $k" =>
        Sue(number, Map(thingOne -> i.toInt, thingTwo -> j.toInt, thingThree -> k.toInt))

  def findSueForTickerTape(sues: List[Sue]): Int =
    sues.filter(sue =>
      sue.things.getOrElse("children", 3) == 3 &&
        sue.things.getOrElse("cats", 7) == 7 &&
        sue.things.getOrElse("samoyeds", 2) == 2 &&
        sue.things.getOrElse("pomeranians", 3) == 3 &&
        sue.things.getOrElse("akitas", 0) == 0 &&
        sue.things.getOrElse("vizlas", 0) == 0 &&
        sue.things.getOrElse("goldfish", 5) == 5 &&
        sue.things.getOrElse("trees", 3) == 3 &&
        sue.things.getOrElse("cars", 2) == 2 &&
        sue.things.getOrElse("perfumes", 1) == 1
    ).head._1.toInt

  def findSueForTickerTapePartTwo(sues: List[Sue]): Int =
    sues.filter(sue =>
      sue.things.getOrElse("children", 3) == 3 &&
        sue.things.getOrElse("cats", 8) > 7 &&
        sue.things.getOrElse("samoyeds", 2) == 2 &&
        sue.things.getOrElse("pomeranians", 0) < 3 &&
        sue.things.getOrElse("akitas", 0) == 0 &&
        sue.things.getOrElse("vizlas", 0) == 0 &&
        sue.things.getOrElse("goldfish", 0) < 5 &&
        sue.things.getOrElse("trees", 4) > 3 &&
        sue.things.getOrElse("cars", 2) == 2 &&
        sue.things.getOrElse("perfumes", 1) == 1
    ).head._1.toInt
  def findSueForInputFile: Int                          =
    Using.resource(Source.fromResource("2015/day16input.txt")): source =>
      val sues = source.getLines().map(handleLine).toList
      findSueForTickerTape(sues)

  def findSueForInputFilePartTwo: Int =
    Using.resource(Source.fromResource("2015/day16input.txt")): source =>
      val sues = source.getLines().map(handleLine).toList
      findSueForTickerTapePartTwo(sues)
end Day16
