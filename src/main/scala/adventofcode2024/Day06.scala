package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06:
  case class Obstacle(x: Int, y: Int)

  case class Guard(x: Int, y: Int, direction: Char)

  def countDistinctPositions(obstacles: Seq[Obstacle], guard: Guard, width: Int, height: Int): Int =
    @tailrec
    def loop(guard: Guard, acc: Set[(Int, Int)]): Int =
      if guard.x < 0 || guard.y < 0 || guard.x == width || guard.y == height then
        acc.size
      else
        guard match
          case Guard(x, y, '^') =>
            if obstacles.contains(Obstacle(x, y - 1)) then
              loop(Guard(x, y, '>'), acc)
            else
              loop(Guard(x, y - 1, '^'), acc.+((x, y)))
          case Guard(x, y, 'v') =>
            if obstacles.contains(Obstacle(x, y + 1)) then
              loop(Guard(x, y, '<'), acc)
            else
              loop(Guard(x, y + 1, 'v'), acc.+((x, y)))
          case Guard(x, y, '>') =>
            if obstacles.contains(Obstacle(x + 1, y)) then
              loop(Guard(x, y, 'v'), acc)
            else
              loop(Guard(x + 1, y, '>'), acc.+((x, y)))
          case Guard(x, y, '<') =>
            if obstacles.contains(Obstacle(x - 1, y)) then
              loop(Guard(x, y, '^'), acc)
            else
              loop(Guard(x - 1, y, '<'), acc.+((x, y)))
    loop(guard, Set.empty)

  def causesLoop(obstacles: Seq[Obstacle], guard: Guard, width: Int, height: Int): Boolean =
    @tailrec
    def loop(guard: Guard, acc: Set[(Int, Int, Char)]): Boolean =
      if guard.x < 0 || guard.y < 0 || guard.x == width || guard.y == height then
        false
      else
        guard match
          case Guard(x, y, '^') =>
            if obstacles.contains(Obstacle(x, y - 1)) then
              loop(Guard(x, y, '>'), acc)
            else if acc == acc.+((x, y, '^')) then true
            else
              loop(Guard(x, y - 1, '^'), acc.+((x, y, '^')))
          case Guard(x, y, 'v') =>
            if obstacles.contains(Obstacle(x, y + 1)) then
              loop(Guard(x, y, '<'), acc)
            else if acc == acc.+((x, y, 'v')) then true
            else
              loop(Guard(x, y + 1, 'v'), acc.+((x, y, 'v')))
          case Guard(x, y, '>') =>
            if obstacles.contains(Obstacle(x + 1, y)) then
              loop(Guard(x, y, 'v'), acc)
            else if acc == acc.+((x, y, '>')) then true
            else
              loop(Guard(x + 1, y, '>'), acc.+((x, y, '>')))
          case Guard(x, y, '<') =>
            if obstacles.contains(Obstacle(x - 1, y)) then
              loop(Guard(x, y, '^'), acc)
            else if acc == acc.+((x, y, '<')) then true
            else
              loop(Guard(x - 1, y, '<'), acc.+((x, y, '<')))

    loop(guard, Set.empty)

  def countObstructionsThatCauseLoop(obstacles: Seq[Obstacle], guard: Guard, width: Int, height: Int): Int =
    val possibleObstructions =
      for
        x <- 0 until width
        y <- 0 until height
        if guard != Guard(x, y, '^')
        if !obstacles.contains(Obstacle(x, y))
      yield Obstacle(x, y)
    possibleObstructions.count(obstruction => causesLoop(obstacles :+ obstruction, guard, width, height))

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(obstacles: Seq[Obstacle], guard: Guard, width: Int, height: Int): String =
    val grid = createGrid(width, height)
    createGrid(width, height).map:
      case (0, y) => if obstacles.contains(Obstacle(0, y)) then "\n#"
        else if guard.x == 0 && guard.y == y then s"\n${guard.direction}"
        else "\n."
      case (x, y) => if obstacles.contains(Obstacle(x, y)) then "#"
        else if guard.x == x && guard.y == y then guard.direction.toString
        else "."
    .mkString

  def findGuard(line: String, y: Int): Option[Guard] =
    line.zipWithIndex.collectFirst:
      case (char, x) if char == '^' =>
        Guard(x, y, '^')

  def parse(line: String, y: Int): Seq[Obstacle] =
    line.zipWithIndex.collect:
      case (char, x) if char == '#' =>
        Obstacle(x, y)

  def handleLines(lines: List[String]): (Seq[Obstacle], Guard) =
    val obstacles = lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
    val guard     = lines.zipWithIndex.map(l => findGuard(l._1, l._2))
    (obstacles, guard.flatten.head)

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day06
