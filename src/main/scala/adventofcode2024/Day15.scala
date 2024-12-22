package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day15:
  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawWarehouse(things: Map[(Int, Int), Char], width: Int, height: Int): String =
    val wareHouse = createGrid(width, height)
    createGrid(width, height).map:
      case (0, y) => "\n" + things.getOrElse((0, y), '.')
      case (x, y) => things.getOrElse((x, y), '.')
    .mkString

  @tailrec
  def isBlocked(direction: Char, things: Map[(Int, Int), Char], x: Int, y: Int): Boolean =
    direction match
      case '<' =>
        things.get((x - 1, y)) match
          case Some('#') => true
          case Some('.') => false
          case Some('O') => isBlocked(direction, things, x - 1, y)
      case '>' =>
        things.get((x + 1, y)) match
          case Some('#') => true
          case Some('.') => false
          case Some('O') => isBlocked(direction, things, x + 1, y)
      case '^' =>
        things.get((x, y - 1)) match
          case Some('#') => true
          case Some('.') => false
          case Some('O') => isBlocked(direction, things, x, y - 1)
      case 'v' =>
        things.get((x, y + 1)) match
          case Some('#') => true
          case Some('.') => false
          case Some('O') => isBlocked(direction, things, x, y + 1)

  def isBlockedPart2(direction: Char, things: Map[(Int, Int), Char], x: Int, y: Int): Boolean =
    direction match
      case '<' =>
        things.get((x - 1, y)) match
          case Some('#') => true
          case Some('.') => false
          case Some(']') => isBlockedPart2(direction, things, x - 1, y)
          case Some('[') => isBlockedPart2(direction, things, x - 1, y)
          case None      => false
      case '>' =>
        things.get((x + 1, y)) match
          case Some('#') => true
          case Some('.') => false
          case Some('[') => isBlockedPart2(direction, things, x + 1, y)
          case Some(']') => isBlockedPart2(direction, things, x + 1, y)
          case None      => false
      case '^' =>
        things.get((x, y - 1)) match
          case Some('#') => true
          case Some('.') => false
          case Some('[') =>
            isBlockedPart2(direction, things, x, y - 1) || isBlockedPart2(direction, things, x + 1, y - 1)
          case Some(']') =>
            isBlockedPart2(direction, things, x, y - 1) || isBlockedPart2(direction, things, x - 1, y - 1)
          case None      => false
      case 'v' =>
        things.get((x, y + 1)) match
          case Some('#') => true
          case Some('.') => false
          case Some('[') =>
            isBlockedPart2(direction, things, x, y + 1) || isBlockedPart2(direction, things, x + 1, y + 1)
          case Some(']') =>
            isBlockedPart2(direction, things, x, y + 1) || isBlockedPart2(direction, things, x - 1, y + 1)
          case None      => false

  def moveThing(direction: Char, things: Map[(Int, Int), Char], x: Int, y: Int): Map[(Int, Int), Char] =
    val thing = things.get((x, y))
    direction match
      case '<' =>
        things.get((x - 1, y)) match
          case Some('#') => things
          case Some('.') => things.removed((x - 1, y)).removed((x, y)) + ((x - 1, y) -> thing.get) + ((x, y) -> '.')
          case Some('O') =>
            moveThing(direction, things, x - 1, y).removed((x, y)) + ((x - 1, y) -> thing.get) + ((x, y) -> '.')
      case '>' =>
        things.get((x + 1, y)) match
          case Some('#') => things
          case Some('.') => things.removed((x + 1, y)).removed((x, y)) + ((x + 1, y) -> thing.get) + ((x, y) -> '.')
          case Some('O') =>
            moveThing(direction, things, x + 1, y).removed((x, y)) + ((x + 1, y) -> thing.get) + ((x, y) -> '.')
      case '^' =>
        things.get((x, y - 1)) match
          case Some('#') => things
          case Some('.') => things.removed((x, y - 1)).removed((x, y)) + ((x, y - 1) -> thing.get) + ((x, y) -> '.')
          case Some('O') =>
            moveThing(direction, things, x, y - 1).removed((x, y - 1)) + ((x, y - 1) -> thing.get) + ((x, y) -> '.')
      case 'v' =>
        things.get((x, y + 1)) match
          case Some('#') => things
          case Some('.') => things.removed((x, y + 1)).removed((x, y)) + ((x, y + 1) -> thing.get) + ((x, y) -> '.')
          case Some('O') =>
            moveThing(direction, things, x, y + 1).removed((x, y + 1)) + ((x, y + 1) -> thing.get) + ((x, y) -> '.')

  def moveThingPart2(direction: Char, things: Map[(Int, Int), Char], x: Int, y: Int): Map[(Int, Int), Char] =
    val thing = things.get((x, y))
    direction match
      case '<' =>
        things.get((x - 1, y)) match
          case Some('#') => things
          case Some('.') => things.removed((x - 1, y)).removed((x, y)) + ((x - 1, y) -> thing.get) + ((x, y) -> '.')
          case Some('[') =>
            moveThingPart2(direction, things, x - 1, y).removed((x, y)) + ((x - 1, y) -> thing.get) + ((x, y) -> '.')
          case Some(']') =>
            moveThingPart2(direction, things, x - 1, y).removed((x, y)) + ((x - 1, y) -> thing.get) + ((x, y) -> '.')
          case None      => things.removed((x - 1, y)).removed((x, y)) + ((x - 1, y) -> thing.get) + ((x, y) -> '.')
      case '>' =>
        things.get((x + 1, y)) match
          case Some('#') => things
          case Some('.') => things.removed((x + 1, y)).removed((x, y)) + ((x + 1, y) -> thing.get) + ((x, y) -> '.')
          case Some('[') =>
            moveThingPart2(direction, things, x + 1, y).removed((x, y)) + ((x + 1, y) -> thing.get) + ((x, y) -> '.')
          case Some(']') =>
            moveThingPart2(direction, things, x + 1, y).removed((x, y)) + ((x + 1, y) -> thing.get) + ((x, y) -> '.')
          case None      => things.removed((x + 1, y)).removed((x, y)) + ((x + 1, y) -> thing.get) + ((x, y) -> '.')
      case '^' =>
        things.get((x, y - 1)) match
          case Some('#') => things
          case Some('.') => things.removed((x, y - 1)).removed((x, y)) + ((x, y - 1) -> thing.get) + ((x, y) -> '.')
          case Some('[') =>
            val newThings = moveThingPart2(direction, things, x, y - 1).removed((x, y - 1))
            moveThingPart2(direction, newThings, x + 1, y - 1).removed((x + 1, y - 1)) + ((x, y - 1) -> thing.get) + ((
              x,
              y
            )                                                                                        -> '.')
          case Some(']') =>
            val newThings = moveThingPart2(direction, things, x, y - 1).removed((x, y - 1))
            moveThingPart2(direction, newThings, x - 1, y - 1).removed((x - 1, y - 1)) + ((x, y - 1) -> thing.get) + ((
              x,
              y
            )                                                                                        -> '.')
          case None      => things.removed((x, y - 1)).removed((x, y)) + ((x, y - 1) -> thing.get) + ((x, y) -> '.')
      case 'v' =>
        things.get((x, y + 1)) match
          case Some('#') => things
          case Some('.') => things.removed((x, y + 1)).removed((x, y)) + ((x, y + 1) -> thing.get) + ((x, y) -> '.')
          case Some('[') =>
            val newThings = moveThingPart2(direction, things, x, y + 1).removed((x, y + 1))
            moveThingPart2(direction, newThings, x + 1, y + 1).removed((x + 1, y + 1)) + ((x, y + 1) -> thing.get) + ((
              x,
              y
            )                                                                                        -> '.')
          case Some(']') =>
            val newThings = moveThingPart2(direction, things, x, y + 1).removed((x, y + 1))
            moveThingPart2(direction, newThings, x - 1, y + 1).removed((x - 1, y + 1)) + ((x, y + 1) -> thing.get) + ((
              x,
              y
            )                                                                                        -> '.')
          case None      => things.removed((x, y + 1)).removed((x, y)) + ((x, y + 1) -> thing.get) + ((x, y) -> '.')

  @tailrec
  def walkRobot(things: Map[(Int, Int), Char], moves: String): Map[(Int, Int), Char] =
    if moves.isEmpty then
      things
    else
      val robot     = things.find(_._2 == '@').get
      val blocked   = isBlocked(moves.head, things, robot._1._1, robot._1._2)
      val newThings = if !blocked then moveThing(moves.head, things, robot._1._1, robot._1._2) else things

      walkRobot(newThings, moves.tail)

  @tailrec
  def walkRobotPartTwo(things: Map[(Int, Int), Char], moves: String): Map[(Int, Int), Char] =
    if moves.isEmpty then
      things
    else
      val robot     = things.find(_._2 == '@').get
      val blocked   = isBlockedPart2(moves.head, things, robot._1._1, robot._1._2)
      val newThings = if !blocked then moveThingPart2(moves.head, things, robot._1._1, robot._1._2) else things
      walkRobotPartTwo(newThings, moves.tail)

  private def calculateGPSCoordinate(x: Int, y: Int): Int = 100 * y + x

  def sumAllBoxesGPSCoordinates(things: Map[(Int, Int), Char], moves: String): Int =
    walkRobot(things, moves)
      .collect:
        case thing if thing._2 == 'O' => calculateGPSCoordinate(thing._1._1, thing._1._2)
      .sum

  def sumAllBoxesGPSCoordinatesPartTwo(things: Map[(Int, Int), Char], moves: String): Int =
    walkRobotPartTwo(things, moves)
      .collect:
        case thing if thing._2 == '[' => calculateGPSCoordinate(thing._1._1, thing._1._2)
      .sum

  def parse(line: String, y: Int): Map[(Int, Int), Char] =
    line.zipWithIndex.collect:
      case (c, x) => (x, y) -> c
    .toMap

  def parsePartTwo(line: String, y: Int): Map[(Int, Int), Char] =
    val expandedLine = line.map:
      case '#' => "##"
      case 'O' => "[]"
      case '.' => ".."
      case '@' => "@."
    expandedLine.mkString.zipWithIndex.collect:
      case (c, x) => (x, y) -> c
    .toMap

  def handleLines(lines: List[String]): (Map[(Int, Int), Char], String) =
    lines.splitAt(lines.indexWhere(_.isEmpty)).match
      case (x, y) => (x.zipWithIndex.flatMap(l => parse(l._1, l._2)).toMap, y.mkString)

  def handleLinesPartTwo(lines: List[String]): (Map[(Int, Int), Char], String) =
    lines.splitAt(lines.indexWhere(_.isEmpty)).match
      case (x, y) => (x.zipWithIndex.flatMap(l => parsePartTwo(l._1, l._2)).toMap, y.mkString)

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day15
