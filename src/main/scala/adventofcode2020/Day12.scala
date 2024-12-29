package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

import adventofcode2020.Day12.Direction.*

object Day12:
  enum Direction:
    case North extends Direction
    case East  extends Direction
    case South extends Direction
    case West  extends Direction
  end Direction

  object Direction:
    def opposite(dir: Direction): Direction =
      dir match
        case North => South
        case South => North
        case East  => West
        case West  => East

    def cw(dir: Direction): Direction =
      dir match
        case North => East
        case South => West
        case East  => South
        case West  => North
  end Direction

  def moveShip(instructions: List[String]): Int =

    @tailrec
    def loop(instructions: List[String], facing: Direction, x: Int, y: Int): Int =
      if instructions.isEmpty then
        math.abs(x) + math.abs(y)
      else
        instructions.head match
          case s"N$value"                       => loop(instructions.tail, facing, x, y - value.toInt)
          case s"S$value"                       => loop(instructions.tail, facing, x, y + value.toInt)
          case s"E$value"                       => loop(instructions.tail, facing, x + value.toInt, y)
          case s"W$value"                       => loop(instructions.tail, facing, x - value.toInt, y)
          case s"R$value" if value.toInt == 90  => loop(instructions.tail, cw(facing), x, y)
          case s"R$value" if value.toInt == 180 => loop(instructions.tail, opposite(facing), x, y)
          case s"R$value" if value.toInt == 270 => loop(instructions.tail, cw(opposite(facing)), x, y)
          case s"L$value" if value.toInt == 90  => loop(instructions.tail, cw(opposite(facing)), x, y)
          case s"L$value" if value.toInt == 180 => loop(instructions.tail, opposite(facing), x, y)
          case s"L$value" if value.toInt == 270 => loop(instructions.tail, cw(facing), x, y)
          case s"F$value" if facing == East     => loop(instructions.tail, facing, x + value.toInt, y)
          case s"F$value" if facing == West     => loop(instructions.tail, facing, x - value.toInt, y)
          case s"F$value" if facing == North    => loop(instructions.tail, facing, x, y - value.toInt)
          case s"F$value" if facing == South    => loop(instructions.tail, facing, x, y + value.toInt)

    loop(instructions, East, 0, 0)

  def moveWaypointAndShip(instructions: List[String]): Int =

    @tailrec
    def loop(instructions: List[String], facing: Direction, x: Int, y: Int, shipX: Int, shipY: Int): Int =
      if instructions.isEmpty then
        math.abs(shipX) + math.abs(shipY)
      else
        instructions.head match
          case s"N$value"                       => loop(instructions.tail, facing, x, y - value.toInt, shipX, shipY)
          case s"S$value"                       => loop(instructions.tail, facing, x, y + value.toInt, shipX, shipY)
          case s"E$value"                       => loop(instructions.tail, facing, x + value.toInt, y, shipX, shipY)
          case s"W$value"                       => loop(instructions.tail, facing, x - value.toInt, y, shipX, shipY)
          case s"R$value" if value.toInt == 90  => loop(instructions.tail, facing, y * -1, x, shipX, shipY)
          case s"R$value" if value.toInt == 180 => loop(instructions.tail, facing, x * -1, y * -1, shipX, shipY)
          case s"R$value" if value.toInt == 270 => loop(instructions.tail, facing, y, x * -1, shipX, shipY)
          case s"L$value" if value.toInt == 90  => loop(instructions.tail, facing, y, x * -1, shipX, shipY)
          case s"L$value" if value.toInt == 180 => loop(instructions.tail, facing, x * -1, y * -1, shipX, shipY)
          case s"L$value" if value.toInt == 270 => loop(instructions.tail, facing, y * -1, x, shipX, shipY)
          case s"F$value"                       => loop(instructions.tail, facing, x, y, shipX + (value.toInt * x), shipY + (value.toInt * y))

    loop(instructions, East, 10, -1, 0, 0)

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day12
