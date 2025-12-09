package adventofcode2025

import scala.io.Source
import scala.util.Using

object Day09:
  case class Pos(x: Int, y: Int):
    def area(otherPos: Pos): Long =
      (math.abs(x - otherPos.x).toLong + 1) * (math.abs(y - otherPos.y).toLong + 1)

  def largestSquare(positions: List[Pos]): Long =
    positions.combinations(2).map: combo =>
      combo.head.area(combo.last)
    .max

  def edgeCrossing(point: Pos, edge: (Pos, Pos)): Int =
    // adjusted even-odd counting, based on this post:
    // https://stackoverflow.com/questions/52731700/how-does-the-even-odd-algorithm-count-polygon-edges
    if edge._1.x == edge._2.x &&                           // no horizontal edges
      point.x < edge._1.x &&
      point.y >= math.min(edge._1.y + 1, edge._2.y + 1) && // do not include top-most point
      point.y <= math.max(edge._1.y, edge._2.y)
    then
      1
    else
      0

  def onBorder(point: Pos, edge: (Pos, Pos)): Boolean =
    (edge._1.x == edge._2.x && point.x == edge._1.x && point.y >= math.min(edge._1.y, edge._2.y) && point.y <= math.max(
      edge._1.y,
      edge._2.y
    )) ||
      (edge._2.y == edge._2.y && point.y == edge._1.y && point.x >= math.min(edge._1.x, edge._2.x) && point.x <= math.max(
        edge._1.x,
        edge._2.x
      ))

  def verifyOtherCorners(edges: List[(Pos, Pos)], combo: List[Pos]): Boolean =
    (edges.exists(edge => onBorder(Pos(combo.head.x, combo.last.y), edge)) ||
      edges.map(edge => edgeCrossing(Pos(combo.head.x, combo.last.y), edge)).sum % 2 == 1) &&
      (edges.exists(edge => onBorder(Pos(combo.last.x, combo.head.y), edge)) ||
        edges.map(edge => edgeCrossing(Pos(combo.last.x, combo.head.y), edge)).sum % 2 == 1)

  def verifyPointInMiddle(edges: List[(Pos, Pos)], combo: List[Pos]): Boolean =
    edges.exists(edge => onBorder(Pos((combo.head.x + combo.last.x) / 2, (combo.head.y + combo.last.y) / 2), edge)) ||
      edges.map(edge => edgeCrossing(Pos((combo.head.x + combo.last.x) / 2, (combo.head.y + combo.last.y) / 2), edge))
        .sum % 2 == 1

  def verifyRandomPoints(edges: List[(Pos, Pos)], combo: List[Pos], n: Int): Boolean =
    var valid = true
    var i     = 0
    while i < n do
      i += 1
      val randomX =
        scala.util.Random.between(math.min(combo.head.x, combo.last.x), math.max(combo.head.x, combo.last.x) + 1)
      val randomY =
        scala.util.Random.between(math.min(combo.head.y, combo.last.y), math.max(combo.head.y, combo.last.y) + 1)
      valid = valid && (edges.exists(edge => onBorder(Pos(randomX, randomY), edge)) ||
        edges.map(edge => edgeCrossing(Pos(randomX, randomY), edge)).sum % 2 == 1)
    valid

  def verifyCombo(edges: List[(Pos, Pos)], combo: List[Pos]): Boolean =
    println(s"verifying combo: $combo with area ${combo.head.area(combo.last)}") // so we don't have to wait 1,5 hr
    var valid = true
    if valid then
      for
        x <- combo.minBy(_.x).x.to(combo.maxBy(_.x).x)
        y <- combo.minBy(_.y).y.to(combo.maxBy(_.y).y)
      do
        valid = valid && {
          val isOnBorder = edges.exists(edge => onBorder(Pos(x, y), edge))
          isOnBorder || edges.map(edge => edgeCrossing(Pos(x, y), edge)).sum % 2 == 1
        }
    valid

  def largestSquareInside(positions: List[Pos]): Long =
    val edges = (positions.sliding(2).toList :+ List(positions.last, positions.head)).map(slide =>
      (slide.head, slide.last)
    )
    positions.combinations(2).toList           // 122760 combinations
      .filter: combo =>
        verifyOtherCorners(edges, combo)       // 52764 combinations
      .filter: combo =>
        verifyPointInMiddle(edges, combo)      // 49086 combinations
      .filter: combo =>
        verifyRandomPoints(edges, combo, 1500) // ~ 1100 combinations
      .map: combo =>
        (combo, combo.head.area(combo.last))
      .sortBy(-_._2)
      .collectFirst:
        case (combo, area) if verifyCombo(edges, combo) => area // takes around 1,5 hr. :(
      .get

  def parseAsGrid(input: String): Array[Array[Char]] =
    val positions = input.split('\n').toList.map:
      case s"$x,$y" => Pos(x.toInt, y.toInt)
    val grid      = Array.fill(positions.maxBy(_.y).y + 1)(Array.fill(positions.maxBy(_.x).x + 1)('.'))
    for
      pos <- positions
    do
      grid(pos.y)(pos.x) = '#'
    (positions.sliding(2).toList :+ List(positions.last, positions.head)).foreach: slide =>
      if slide.head.x == slide.last.x then
        for
          y <- slide.minBy(_.y).y.to(slide.maxBy(_.y).y)
          if y != slide.minBy(_.y).y
          if y != slide.maxBy(_.y).y
        do
          grid(y)(slide.head.x) = 'O'
      if slide.head.y == slide.last.y then
        for
          x <- slide.minBy(_.x).x.to(slide.maxBy(_.x).x)
          if x != slide.minBy(_.x).x
          if x != slide.maxBy(_.x).x
        do
          grid(slide.head.y)(x) = 'O'
    grid

  def drawGrid(grid: Array[Array[Char]]): String =
    grid.map(_.mkString).mkString("\n") :+ '\n'

  def parse(input: String): List[Pos] =
    input.split('\n').toList.map:
      case s"$x,$y" => Pos(x.toInt, y.toInt)

  def importLines(): String =
    Using.resource(Source.fromResource("2025/day09input.txt")): source =>
      source.mkString
end Day09
