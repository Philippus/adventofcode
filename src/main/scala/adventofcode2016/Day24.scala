package adventofcode2016

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day24:
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

  def drawGrid(map: Map[(Int, Int), Char], width: Int, height: Int): String =
    val grid = createGrid(width, height)
    createGrid(width, height).map:
      case (0, y) => "\n" + map(0, y)
      case (x, y) => map(x, y)
    .mkString

  def fewestStepsToGoal(map: Map[Pos, Char], start: Char, goal: Char): Long =
    val unvisited: mutable.Map[Pos, Long] = Map(map.find(_._2 == start).get._1 -> 0L).to(mutable.Map)

    val visited = mutable.Map[Pos, Long]()

    var lowest = Long.MaxValue
    while lowest == Long.MaxValue do
      val (currentNode, currentVal) = unvisited.minBy(_._2)
      if map(currentNode) == goal then
        lowest = currentVal
      if map(currentNode.north) != '#' && !visited.contains(currentNode.north) then
        unvisited.update(
          currentNode.north,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.north, Long.MaxValue))
        )
      if map(currentNode.east) != '#' && !visited.contains(currentNode.east) then
        unvisited.update(
          currentNode.east,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.east, Long.MaxValue))
        )
      if map(currentNode.south) != '#' && !visited.contains(currentNode.south) then
        unvisited.update(
          currentNode.south,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.south, Long.MaxValue))
        )
      if map(currentNode.west) != '#' && !visited.contains(currentNode.west) then
        unvisited.update(
          currentNode.west,
          math.min(currentVal + 1, unvisited.getOrElse(currentNode.west, Long.MaxValue))
        )

      unvisited.remove(currentNode)
      visited.update(currentNode, currentVal)

    lowest

  def fewestStepsVisitingAllNonZeroNumbers(map: Map[Pos, Char], returnToZero: Boolean = false): Long =
    def loop(combinations: Seq[(Set[Char], Long)], curr: Char, acc: Long): Long =
      if combinations.isEmpty then
        acc + (if returnToZero then fewestStepsToGoal(map, curr, '0') else 0L)
      else
        val options = combinations.filter(_._1.contains(curr))
        options.map((chars, dist) =>
          loop(
            combinations.filterNot(_._1.contains(curr)),
            chars.filterNot(_.==(curr)).head,
            acc + dist
          )
        ).min

    val combinations =
      for
        x <- '0' to '9'
        y <- x to '9'
        if x != y
        if map.exists(_._2 == x) && map.exists(_._2 == y)
      yield (Set(x, y), fewestStepsToGoal(map, x, y))

    loop(combinations, '0', 0L)

  def parse(line: String, y: Int): Map[Pos, Char] =
    line.zipWithIndex.collect:
      case (char, x) => Pos(x, y) -> char
    .toMap

  def handleLines(lines: List[String]): Map[Pos, Char] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toMap

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2016/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day24
