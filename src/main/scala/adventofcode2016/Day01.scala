package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
object Day01:
  case class Instruction(turn: Char, blocks: Int)

  case class Location(latitude: Int, longitude: Int)

  def rose(facing: Char, direction: Char): Char =
    (facing, direction) match
      case ('N', 'R') => 'E'
      case ('N', 'L') => 'W'
      case ('E', 'R') => 'S'
      case ('E', 'L') => 'N'
      case ('S', 'R') => 'W'
      case ('S', 'L') => 'E'
      case ('W', 'R') => 'N'
      case ('W', 'L') => 'S'

  def followInstructions(instructions: Seq[Instruction]): Int =
    @tailrec
    def loop(instructions: Seq[Instruction], facing: Char, north: Int, south: Int, east: Int, west: Int): Int =
      if instructions.isEmpty then
        math.abs(north - south) + math.abs(east - west)
      else
        val instruction = instructions.head
        val direction   = rose(facing, instruction.turn)
        direction match
          case 'N' =>
            loop(instructions.tail, direction, north + instruction.blocks, south, east, west)
          case 'S' =>
            loop(instructions.tail, direction, north, south + instruction.blocks, east, west)
          case 'E' =>
            loop(instructions.tail, direction, north, south, east + instruction.blocks, west)
          case 'W' =>
            loop(instructions.tail, direction, north, south, east, west + instruction.blocks)

    loop(instructions, 'N', 0, 0, 0, 0)

  def followInstructionsPartTwo(instructions: Seq[Instruction]): Int =
    @tailrec
    def loop(
        instructions: Seq[Instruction],
        facing: Char,
        north: Int,
        south: Int,
        east: Int,
        west: Int,
        acc: Seq[Location]
    ): Int =
      val instruction  = instructions.head
      val direction    = rose(facing, instruction.turn)
      var blocks       = instruction.blocks
      var n            = north
      var s            = south
      var e            = east
      var w            = west
      var newLocations = Seq.empty[Location]
      while (blocks > 0)
        direction match
          case 'N' => n += 1
          case 'S' => s += 1
          case 'E' => e += 1
          case 'W' => w += 1
        val location = Location(n - s, e - w)
        newLocations = newLocations :+ location
        blocks -= 1

      if acc.intersect(newLocations).nonEmpty then
        val firstLoc = acc.intersect(newLocations).head
        math.abs(firstLoc.latitude) + math.abs(firstLoc.longitude)
      else
        direction match
          case 'N' =>
            loop(instructions.tail, direction, north + instruction.blocks, south, east, west, acc ++ newLocations)
          case 'S' =>
            loop(instructions.tail, direction, north, south + instruction.blocks, east, west, acc ++ newLocations)
          case 'E' =>
            loop(instructions.tail, direction, north, south, east + instruction.blocks, west, acc ++ newLocations)
          case 'W' =>
            loop(instructions.tail, direction, north, south, east, west + instruction.blocks, acc ++ newLocations)

    loop(instructions, 'N', 0, 0, 0, 0, Seq.empty[Location])

  def handlLine(s: String): Seq[Instruction] =
    s.split(", ").map(s => Instruction(s.head, s.tail.toInt))

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day01input.txt")): source =>
      source.getLines().toSeq
end Day01
