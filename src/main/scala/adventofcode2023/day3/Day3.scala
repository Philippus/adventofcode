package adventofcode2023.day3

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day3:
  case class Pos(x: Int, y: Int)
  case class Number(value: String, positions: Seq[Pos])

  def isSymbol(c: Char): Boolean =
    c != '.'

  def isGear(c: Char): Boolean =
    c == '*'

  def findSymbols(line: String, y: Int): Seq[Pos] =
    @tailrec
    def walkLine(line: String, pos: Pos, acc: Seq[Pos]): Seq[Pos] =
      if line.isEmpty then
        acc
      else if isSymbol(line.head) && !line.head.isDigit then
        walkLine(line.tail, pos.copy(x = pos.x + 1), acc :+ pos)
      else
        walkLine(line.tail, pos.copy(x = pos.x + 1), acc)

    walkLine(line, Pos(0, y), Seq.empty[Pos])

  def findGears(line: String, y: Int): Seq[Pos] =
    @tailrec
    def walkLine(line: String, pos: Pos, acc: Seq[Pos]): Seq[Pos] =
      if line.isEmpty then
        acc
      else if isGear(line.head) && !line.head.isDigit then
        walkLine(line.tail, pos.copy(x = pos.x + 1), acc :+ pos)
      else
        walkLine(line.tail, pos.copy(x = pos.x + 1), acc)

    walkLine(line, Pos(0, y), Seq.empty[Pos])

  def findNumbers(line: String, y: Int): Seq[Number] =
    @tailrec
    def walkLine(line: String, pos: Pos, acc: Seq[Number]): Seq[Number] =
      if line.isEmpty then
        acc
      else if line.head.isDigit then
        if acc.lastOption.exists(x => x.positions.lastOption.exists(_.x == pos.x - 1))
        then
          walkLine(
            line.tail,
            pos.copy(x = pos.x + 1),
            acc.updated(
              acc.length - 1,
              Number(acc.last.value + line.head.toString, acc.last.positions :+ Pos(pos.x, y))
            )
          )
        else walkLine(line.tail, pos.copy(x = pos.x + 1), acc :+ Number(line.head.toString, Seq(Pos(pos.x, y))))
      else
        walkLine(line.tail, pos.copy(x = pos.x + 1), acc)

    walkLine(line, Pos(0, y), Seq.empty[Number])

  def filterAdjacent(numbers: Seq[Number], symbolPositions: Seq[Pos]): Seq[Number] =
    for
      number         <- numbers
      symbolPosition <- symbolPositions // number is valid if it has an adjacent symbol
      if number.positions.exists(pos =>
        symbolPosition.x >= pos.x - 1 && symbolPosition.x <= pos.x + 1 &&
          symbolPosition.y >= pos.y - 1 && symbolPosition.y <= pos.y + 1
      )
    yield number

  def findGearRatios(gears: Seq[Pos], numbers: Seq[Number]): Seq[Int] =
    for
      gear           <- gears
      adjacentNumbers = filterAdjacent(numbers, Seq(gear))
      if adjacentNumbers.length > 1
    yield adjacentNumbers.head.value.toInt * adjacentNumbers.last.value.toInt

  def readInputDocument: Int =
    val symbols = Using.resource(Source.fromResource("day3input.txt")): source =>
      val lines   = source.getLines
      var y       = 0
      var symbols = Seq.empty[Pos]
      while (lines.hasNext)
        symbols = symbols ++ findSymbols(lines.next, y)
        y += 1
      symbols

    val numbers = Using.resource(Source.fromResource("day3input.txt")): source =>
      val lines   = source.getLines
      var y       = 0
      var numbers = Seq.empty[Number]
      while (lines.hasNext)
        numbers = numbers ++ findNumbers(lines.next, y)
        y += 1
      numbers

    filterAdjacent(numbers, symbols).map(_.value.toInt).sum

  def readInputDocumentForGearRatios: Int =
    val gears = Using.resource(Source.fromResource("day3input.txt")): source =>
      val lines = source.getLines
      var y     = 0
      var gears = Seq.empty[Pos]
      while (lines.hasNext)
        gears = gears ++ findGears(lines.next, y)
        y += 1
      gears

    val numbers = Using.resource(Source.fromResource("day3input.txt")): source =>
      val lines   = source.getLines
      var y       = 0
      var numbers = Seq.empty[Number]
      while (lines.hasNext)
        numbers = numbers ++ findNumbers(lines.next, y)
        y += 1
      numbers

    findGearRatios(gears, numbers).sum
end Day3
