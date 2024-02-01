package adventofcode2022

import scala.io.Source
import scala.util.Using

object Day08:
  def isVisible(x: Int, y: Int, map: Vector[Vector[Int]]): Boolean =
    // left to right
    val ltr = (for
      i <- 0.until(x)
    yield map(y)(x) > map(y)(i)).forall(_.==(true))

    // right to left
    val rtl = (for
      i <- Range.Exclusive(map.head.length - 1, x, -1)
    yield map(y)(x) > map(y)(i)).forall(_.==(true))

    // top to bottom
    val ttb = (for
      i <- 0.until(y)
    yield map(y)(x) > map(i)(x)).forall(_.==(true))

    // bottom to top
    val btt = (for
      i <- Range.Exclusive(map.length - 1, y, -1)
    yield map(y)(x) > map(i)(x)).forall(_.==(true))

    ltr || rtl || ttb || btt

  def visibleTrees(map: Vector[Vector[Int]]): Int =
    (for
      x <- map.head.indices
      y <- map.indices
    yield isVisible(x, y, map)).count(_.==(true))

  def calculateScenicScore(x: Int, y: Int, map: Vector[Vector[Int]]) =
    // left to right
    var done = false
    var i    = x + 1
    var ltr  = 0
    while (!done && i >= 0 && i < map.head.length)
      if map(y)(x) > map(y)(i) then
        i += 1
        ltr += 1
      else
        ltr += 1
        done = true
    done = false
    i = x - 1
    var rtl  = 0
    while (!done && i >= 0 && i < map.head.length)
      if map(y)(x) > map(y)(i) then
        i -= 1
        rtl += 1
      else
        rtl += 1
        done = true
    done = false
    i = y + 1
    var ttb  = 0
    while (!done && i >= 0 && i < map.length)
      if map(y)(x) > map(i)(x) then
        i += 1
        ttb += 1
      else
        ttb += 1
        done = true
    done = false
    i = y - 1
    var btt  = 0
    while (!done && i >= 0 && i < map.length)
      if map(y)(x) > map(i)(x) then
        i -= 1
        btt += 1
      else
        btt += 1
        done = true

    ltr * rtl * ttb * btt

  def maxScenicScore(map: Vector[Vector[Int]]): Int =
    (for
      x <- map.head.indices
      y <- map.indices
    yield calculateScenicScore(x, y, map)).max

  def handleLines(s: Seq[Seq[Int]]): Vector[Vector[Int]] =
    Vector(s.map(Vector(_*))*)

  def importLines(): Seq[Seq[Int]] =
    Using.resource(Source.fromResource("2022/day08input.txt")): source =>
      source.getLines().toSeq.map(_.map(_.toInt))
end Day08
