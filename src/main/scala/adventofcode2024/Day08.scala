package adventofcode2024

import scala.io.Source
import scala.util.Using

object Day08:
  case class Antenna(x: Int, y: Int, symbol: Char)

  case class Antinode(x: Int, y: Int, symbol: Char = '#')

  def createAntinodesForPair(a: Antenna, b: Antenna, width: Int, height: Int): Set[Antinode] =
    val (diffX, diffY) = (a.x - b.x, a.y - b.y)
    Set(Antinode(a.x + diffX, a.y + diffY), Antinode(b.x - diffX, b.y - diffY))
      .filterNot(a => a.x < 0 || a.y < 0 || a.x > width - 1 || a.y > height - 1)

  def countDistinctAntinodesOnMap(antennas: Seq[Antenna], width: Int, height: Int): Int =
    val antennasGroupedBySymbol = antennas.groupBy(_.symbol)
    val pairsBySymbol           = antennasGroupedBySymbol.values.toSeq.flatMap(_.combinations(2).toSeq)
    pairsBySymbol.flatMap(pair => createAntinodesForPair(pair.head, pair.last, width: Int, height: Int)).toSet.size

  def createAntinodesWithResonantHarmonicsForPair(
      a: Antenna,
      b: Antenna,
      i: Int,
      width: Int,
      height: Int
  ): Set[Antinode] =
    val (diffX, diffY) = (a.x - b.x, a.y - b.y)
    Set(Antinode(a.x + diffX * i, a.y + diffY * i), Antinode(b.x - diffX * i, b.y - diffY * i))
      .filterNot(a => a.x < 0 || a.y < 0 || a.x > width - 1 || a.y > height - 1)

  def countDistinctAntinodesWithResonantHarmonicsOnMap(antennas: Seq[Antenna], width: Int, height: Int): Int =
    val antennasGroupedBySymbol = antennas.groupBy(_.symbol)
    val pairsBySymbol           = antennasGroupedBySymbol.values.toSeq.flatMap(_.combinations(2).toSeq)
    pairsBySymbol.flatMap(pair =>
      (for
        i <- 0 to 50
      yield createAntinodesWithResonantHarmonicsForPair(pair.head, pair.last, i, width: Int, height: Int))
        .flatten
    ).toSet.size

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(antennas: Seq[Antenna], antinodes: Seq[Antinode], width: Int, height: Int): String =
    val grid = createGrid(width, height)
    createGrid(width, height).map:
      case (0, y) => "\n" + antennas.find(a => a.x == 0 && a.y == y).map(_.symbol).orElse(antinodes.find(a =>
          a.x == 0 && a.y == y
        ).map(_.symbol)).getOrElse(".")
      case (x, y) => antennas.find(a => a.x == x && a.y == y).map(_.symbol).orElse(antinodes.find(a =>
          a.x == x && a.y == y
        ).map(_.symbol)).getOrElse(".")
    .mkString

  def parse(line: String, y: Int): Seq[Antenna] =
    line.zipWithIndex.collect:
      case (char, x) if char != '.' => Antenna(x, y, char)

  def handleLines(lines: List[String]): Seq[Antenna] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day08
