package adventofcode2015

import scala.io.Source
import scala.util.Using

object Day09:
  case class Leg(locations: Set[String], distance: Int)

  def lineToLeg(line: String): Leg =
    line match
      case s"$s to $d = $dist" => Leg(Set(s, d), dist.toInt)

  def distanceForRoute(route: Seq[String], legs: Set[Leg]): Int =
    route.sliding(2).map(s => legs.find(_.locations == Set(s.head, s.last)).map(_.distance).get).sum

  def routes(legs: Set[Leg]): Iterator[Int] =
    val locations    = legs.flatMap(_.locations)
    val permutations = locations.toSeq.permutations
    permutations.map(p => distanceForRoute(p, legs))

  def shortestRoute(legs: Set[Leg]): Int =
    routes(legs).min

  def longestRoute(legs: Set[Leg]): Int =
    routes(legs).max

  def calculateShortestRouteForFile: Int =
    Using.resource(Source.fromResource("2015/day09input.txt")): source =>
      shortestRoute(source.getLines().map(lineToLeg).toSet)

  def calculateLongestRouteForFile: Int =
    Using.resource(Source.fromResource("2015/day09input.txt")): source =>
      longestRoute(source.getLines().map(lineToLeg).toSet)
end Day09
