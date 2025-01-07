package adventofcode2021

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day15:
  case class Pos(x: Int, y: Int):
    def north: Pos = Pos(x, y - 1)
    def east: Pos  = Pos(x + 1, y)
    def south: Pos = Pos(x, y + 1)
    def west: Pos  = Pos(x - 1, y)
  end Pos

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(map: Map[Pos, Int], width: Int, height: Int): String =
    createGrid(width, height).map:
      case (x, y) => (if x == 0 then "\n" else "") + map(Pos(x, y))
    .mkString

  def lowestRisk(map: Map[Pos, Int]): Long =
    val goal                              = Pos(map.maxBy(_._1.x)._1.x, map.maxBy(_._1.y)._1.y)
    val unvisited: mutable.Map[Pos, Long] =
      map.map((p, c) => if p == Pos(0, 0) then p -> 0L else p -> Long.MaxValue).to(mutable.Map)

    val visited = mutable.Map[Pos, Long]()

    var lowestRisk = Long.MaxValue
    while lowestRisk == Long.MaxValue do
      val (currentNode, currentVal) = unvisited.minBy(_._2)
      if currentNode == goal then
        lowestRisk = currentVal
      if unvisited.contains(currentNode.north) then
        unvisited.update(currentNode.north, math.min(currentVal + map(currentNode.north), unvisited(currentNode.north)))
      if unvisited.contains(currentNode.east) then
        unvisited.update(currentNode.east, math.min(currentVal + map(currentNode.east), unvisited(currentNode.east)))
      if unvisited.contains(currentNode.south) then
        unvisited.update(currentNode.south, math.min(currentVal + map(currentNode.south), unvisited(currentNode.south)))
      if unvisited.contains(currentNode.west) then
        unvisited.update(currentNode.west, math.min(currentVal + map(currentNode.west), unvisited(currentNode.west)))

      unvisited.remove(currentNode)
      visited.update(currentNode, currentVal)
    lowestRisk

  def expandMap(map: Map[Pos, Int]): Map[Pos, Int] =
    val width  = map.maxBy(_._1.x)._1.x + 1
    val height = map.maxBy(_._1.y)._1.y + 1

    val m10 = map.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m20 = m10.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m30 = m20.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m40 = m30.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)

    val m01 = map.map:
      case (p, i) => (Pos(p.x, p.y + height), if i == 9 then 1 else i + 1)
    val m11 = m01.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m21 = m11.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m31 = m21.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m41 = m31.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)

    val m02 = m01.map:
      case (p, i) => (Pos(p.x, p.y + height), if i == 9 then 1 else i + 1)
    val m12 = m02.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m22 = m12.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m32 = m22.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m42 = m32.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)

    val m03 = m02.map:
      case (p, i) => (Pos(p.x, p.y + height), if i == 9 then 1 else i + 1)
    val m13 = m03.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m23 = m13.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m33 = m23.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m43 = m33.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)

    val m04 = m03.map:
      case (p, i) => (Pos(p.x, p.y + height), if i == 9 then 1 else i + 1)
    val m14 = m04.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m24 = m14.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m34 = m24.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)
    val m44 = m34.map:
      case (p, i) => (Pos(p.x + width, p.y), if i == 9 then 1 else i + 1)

    map ++ m10 ++ m20 ++ m30 ++ m40 ++
      m01 ++ m11 ++ m21 ++ m31 ++ m41 ++
      m02 ++ m12 ++ m22 ++ m32 ++ m42 ++
      m03 ++ m13 ++ m23 ++ m33 ++ m43 ++
      m04 ++ m14 ++ m24 ++ m34 ++ m44

  def parse(line: String, y: Int): Map[Pos, Int] =
    line.zipWithIndex.collect:
      case (char, x) => Pos(x, y) -> char.asDigit
    .toMap

  def handleLines(lines: List[String]): Map[Pos, Int] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toMap

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2021/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day15
