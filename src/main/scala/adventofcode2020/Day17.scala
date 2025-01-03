package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day17:
  case class Cube(x: Int, y: Int, z: Int):
    def neighbours: Seq[Cube] = // cube has 26 neighbours
      (for
        nX <- x - 1 to x + 1
        nY <- y - 1 to y + 1
        nZ <- z - 1 to z + 1
      yield Cube(nX, nY, nZ)).filterNot(_.==(Cube(x, y, z)))
  end Cube

  def createGrid(fromX: Int, toX: Int, fromY: Int, toY: Int): Seq[(Int, Int)] =
    for
      y <- fromY to toY
      x <- fromX to toX
    yield (x, y)

  def drawSlice(state: Map[Cube, Char], z: Int): String =
    val grid = createGrid(
      fromX = state.filter(_._2 == '#').view.minBy(_._1.x)._1.x,
      toX = state.filter(_._2 == '#').view.maxBy(_._1.x)._1.x,
      fromY = state.filter(_._2 == '#').view.minBy(_._1.y)._1.y,
      toY = state.filter(_._2 == '#').view.maxBy(_._1.y)._1.y
    )
    s"\nz=$z" ++
      grid.map:
        case (x, y) =>
          (if state.filter(_._2 == '#').view.minBy(_._1.x)._1.x == x then "\n" else "") +
            state.getOrElse(Cube(x, y, z), '.')
      .mkString

  def simulateCycles(initialState: Map[Cube, Char], cycles: Int): Map[Cube, Char] =
    @tailrec
    def loop(state: Map[Cube, Char], cycle: Int): Map[Cube, Char] =
      if cycle == cycles then
        state
      else
        val expandedPocketDimension =
          (for
            x <- state.minBy(_._1.x)._1.x - 1 to state.maxBy(_._1.x)._1.x + 1
            y <- state.minBy(_._1.y)._1.y - 1 to state.maxBy(_._1.y)._1.y + 1
            z <- state.minBy(_._1.z)._1.z - 1 to state.maxBy(_._1.z)._1.z + 1
          yield Cube(x, y, z) -> '.')
            .toMap ++ state

        val newState = expandedPocketDimension.map: c =>
          if c._2 == '#' then
            val activeNeighbours = c._1.neighbours.count: n =>
              expandedPocketDimension.getOrElse(n, '.') == '#'
            if activeNeighbours == 2 || activeNeighbours == 3 then
              c._1 -> '#'
            else
              c._1 -> '.'
          else
            val activeNeighbours = c._1.neighbours.count: n =>
              expandedPocketDimension.getOrElse(n, '.') == '#'
            if activeNeighbours == 3 then
              c._1 -> '#'
            else
              c._1 -> '.'

        loop(newState, cycle + 1)

    loop(initialState, 0)

  def countActiveCubes(state: Map[Cube, Char] | Map[Hypercube, Char]): Int = state.count(_._2 == '#')

  case class Hypercube(x: Int, y: Int, z: Int, w: Int):
    def neighbours: Seq[Hypercube] = // hypercube has 80 neighbours
      (for
        nX <- x - 1 to x + 1
        nY <- y - 1 to y + 1
        nZ <- z - 1 to z + 1
        nW <- w - 1 to w + 1
      yield Hypercube(nX, nY, nZ, nW)).filterNot(_.==(Hypercube(x, y, z, w)))
  end Hypercube

  def drawHypercubeSlice(state: Map[Hypercube, Char], z: Int, w: Int): String =
    val grid = createGrid(
      fromX = state.minBy(_._1.x)._1.x,
      toX = state.maxBy(_._1.x)._1.x,
      fromY = state.minBy(_._1.y)._1.y,
      toY = state.maxBy(_._1.y)._1.y
    )
    s"\nz=$z, w=$w" ++
      grid.map:
        case (x, y) =>
          (if x == state.minBy(_._1.x)._1.x then "\n" else "") + state.getOrElse(Hypercube(x, y, z, w), '.')
      .mkString

  def simulateCyclesIn4Dimensions(initialState: Map[Hypercube, Char], cycles: Int): Map[Hypercube, Char] =
    @tailrec
    def loop(state: Map[Hypercube, Char], cycle: Int): Map[Hypercube, Char] =
      if cycle == cycles then
        state
      else
        val expandedPocketDimension =
          (for
            x <- state.minBy(_._1.x)._1.x - 1 to state.maxBy(_._1.x)._1.x + 1
            y <- state.minBy(_._1.y)._1.y - 1 to state.maxBy(_._1.y)._1.y + 1
            z <- state.minBy(_._1.z)._1.z - 1 to state.maxBy(_._1.z)._1.z + 1
            w <- state.minBy(_._1.w)._1.w - 1 to state.maxBy(_._1.w)._1.w + 1
          yield Hypercube(x, y, z, w) -> '.').toMap ++ state

        val newState = expandedPocketDimension.map: c =>
          if c._2 == '#' then
            val activeNeighbours = c._1.neighbours.count: n =>
              expandedPocketDimension.getOrElse(n, '.') == '#'
            if activeNeighbours == 2 || activeNeighbours == 3 then
              c._1 -> '#'
            else
              c._1 -> '.'
          else
            val activeNeighbours = c._1.neighbours.count: n =>
              expandedPocketDimension.getOrElse(n, '.') == '#'
            if activeNeighbours == 3 then
              c._1 -> '#'
            else
              c._1 -> '.'

        loop(newState, cycle + 1)

    loop(initialState, 0)

  def parseToCubes(lines: Vector[String]): Map[Cube, Char] =
    lines.zipWithIndex.flatMap:
      case (line, i) => line.zipWithIndex.map:
          case (char, j) => Cube(j, i, 0) -> char
    .toMap

  def parseToHypercubes(lines: Vector[String]): Map[Hypercube, Char] =
    lines.zipWithIndex.flatMap:
      case (line, i) => line.zipWithIndex.map:
          case (char, j) => Hypercube(j, i, 0, 0) -> char
    .toMap

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day17
