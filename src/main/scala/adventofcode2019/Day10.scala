package adventofcode2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10:
  case class Pos(x: Int, y: Int)

  def countDetectedAsteroids(pos: Pos, asteroids: List[Pos]): Int =
    asteroids.filterNot(_.==(pos)).map: asteroid =>
      math.atan2(asteroid.y - pos.y, asteroid.x - pos.x)
    .distinct.length

  def findMaxDetectedAsteroids(asteroids: List[Pos]): Int =
    (for
      asteroid <- asteroids
    yield countDetectedAsteroids(asteroid, asteroids)).max

  def findBestPosition(asteroids: List[Pos]): Pos =
    (for
      asteroid <- asteroids
    yield (asteroid, countDetectedAsteroids(asteroid, asteroids))).maxBy(_._2)._1

  def nthVaporizedAsteroid(pos: Pos, asteroids: List[Pos], n: Int): Pos =
    @tailrec
    def determineVaporizingOrder(
        sortedAsteroids: List[(Pos, Double)],
        acc: List[(Pos, Double)]
    ): List[Pos] =
      if sortedAsteroids.isEmpty then
        acc.map(_._1)
      else if sortedAsteroids.tail.forall(_._2 == sortedAsteroids.head._2)
      then // only asteroids left with the same angle
        (acc ++ sortedAsteroids).map(_._1)
      else
        val asteroid = sortedAsteroids.head
        if acc.isEmpty then
          determineVaporizingOrder(sortedAsteroids.tail, acc :+ asteroid)
        else if asteroid._2 == acc.last._2 then
          determineVaporizingOrder(sortedAsteroids.tail :+ asteroid, acc) // blocked, so put at the end of the queue
        else
          determineVaporizingOrder(sortedAsteroids.tail, acc :+ asteroid)

    def toDegrees(pos: Pos, asteroid: Pos) =
      val degrees = math.toDegrees(math.atan2(asteroid.y - pos.y, asteroid.x - pos.x)) + 90.0D
      if degrees < 0.0D then degrees + 360.0D else degrees

    val asteroidsSortedByAngleAndDistance = (asteroids.filterNot(_.==(pos)).map: asteroid =>
      (asteroid, toDegrees(pos, asteroid))).sortBy((asteroid, angle) =>
      (angle, math.sqrt(math.pow(pos.x - asteroid.x, 2) + math.pow(pos.y - asteroid.y, 2)))
    )
    determineVaporizingOrder(asteroidsSortedByAngleAndDistance, List.empty)(n - 1)

  def twoHundredthVaporizedAsteroid(asteroids: List[Pos]): Int =
    val pos = nthVaporizedAsteroid(findBestPosition(asteroids), asteroids, 200)
    pos.x * 100 + pos.y

  def parse(input: String): List[Pos] =
    def getPositions(map: Array[Array[Char]]): List[Pos] =
      (for
        y <- map.indices
        x <- map(0).indices
        if map(y)(x) == '#'
      yield Pos(x, y)).toList

    getPositions(input.split("\n").map(_.toCharArray))

  def importLines(): String =
    Using.resource(Source.fromResource("2019/day10input.txt")): source =>
      source.mkString
end Day10
