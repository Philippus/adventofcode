package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09:

  def sizeOfBasin(heightMap: Vector[Vector[Int]], lowPoint: (Int, Int)): Int =
    @tailrec
    def loop(pointsToCheck: Seq[(Int, Int)], pointsChecked: Seq[(Int, Int)]): Int =
      if pointsToCheck.isEmpty then
        pointsChecked.distinct.length
      else
        val pointToCheck                      = pointsToCheck.head
        val (x, y)                            = (pointToCheck._2, pointToCheck._1)
        var newPointsToCheck: Seq[(Int, Int)] = Seq.empty
        if !pointsChecked.contains((y, x - 1)) && x - 1 >= 0 && heightMap(y)(x - 1) != 9 then // left
          newPointsToCheck = newPointsToCheck :+ (y, x - 1)
        if !pointsChecked.contains((y, x + 1)) && x + 1 <= heightMap.head.length - 1 && heightMap(y)(x + 1) != 9
        then                                                                                  // right
          newPointsToCheck = newPointsToCheck :+ (y, x + 1)
        if !pointsChecked.contains((y - 1, x)) && y - 1 >= 0 && heightMap(y - 1)(x) != 9 then // above
          newPointsToCheck = newPointsToCheck :+ (y - 1, x)
        if !pointsChecked.contains((y + 1, x)) && y + 1 <= heightMap.length - 1 && heightMap(y + 1)(x) != 9
        then                                                                                  // below
          newPointsToCheck = newPointsToCheck :+ (y + 1, x)
        loop(pointsToCheck.tail ++ newPointsToCheck, pointsChecked :+ pointToCheck)
    loop(Seq(lowPoint), Seq.empty)

  def productOfThreeLargestBasins(heightMap: Vector[Vector[Int]]): Int =
    val lowPoints =
      (for
        x <- heightMap.head.indices
        y <- heightMap.indices
        xy = heightMap(y)(x)
        if (x == 0 || heightMap(y)(x - 1) > xy) &&
          (x == heightMap.head.length - 1 || heightMap(y)(x + 1) > xy) &&
          (y == 0 || heightMap(y - 1)(x) > xy) &&
          (y == heightMap.length - 1 || heightMap(y + 1)(x) > xy)
      yield (y, x))

    (lowPoints.map: lowPoint =>
      sizeOfBasin(heightMap, lowPoint))
    .sorted.takeRight(3).product

  def findSumOfRiskLevels(heightMap: Vector[Vector[Int]]): Int =
    (for
      x <- heightMap.head.indices
      y <- heightMap.indices
      xy = heightMap(y)(x)
      if (x == 0 || heightMap(y)(x - 1) > xy) &&
        (x == heightMap.head.length - 1 || heightMap(y)(x + 1) > xy) &&
        (y == 0 || heightMap(y - 1)(x) > xy) &&
        (y == heightMap.length - 1 || heightMap(y + 1)(x) > xy)
    yield heightMap(y)(x)).map(lp => lp + 1).sum

  def handleLines(s: Seq[String]): Vector[Vector[Int]] =
    Vector(s.map(_.map(_.asDigit).toVector)*)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2021/day09input.txt")): source =>
      source.getLines().toSeq
end Day09
