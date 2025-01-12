package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

import adventofcode2024.Day16.Direction.*

object Day19:
  case class Pos(x: Int, y: Int):
    def up: Pos    = Pos(x, y - 1)
    def right: Pos = Pos(x + 1, y)
    def down: Pos  = Pos(x, y + 1)
    def left: Pos  = Pos(x - 1, y)
  end Pos

  def followPath(map: Map[Pos, Char]): (String, Int) =
    def findStart: Pos =
      map.collectFirst:
        case (pos, c) if pos.y == 0 && c == '|' => pos
      .get

    @tailrec
    def loop(currentPos: Pos, letters: String, steps: Int, dir: Char): (String, Int) =
      (dir, map.getOrElse(currentPos, ' ')) match
        case (_, ' ')         =>
          (letters, steps)
        case ('U', '|' | '-') =>
          loop(currentPos.up, letters, steps + 1, dir)
        case ('U', '+')       =>
          if map.getOrElse(currentPos.right, ' ') != ' ' then
            loop(currentPos.right, letters, steps + 1, 'R')
          else
            loop(currentPos.left, letters, steps + 1, 'L')
        case ('U', letter)    =>
          loop(currentPos.up, letters :+ letter, steps + 1, dir)
        case ('R', '-' | '|') =>
          loop(currentPos.right, letters, steps + 1, dir)
        case ('R', '+')       =>
          if map.getOrElse(currentPos.up, ' ') != ' ' then
            loop(currentPos.up, letters, steps + 1, 'U')
          else
            loop(currentPos.down, letters, steps + 1, 'D')
        case ('R', letter)    =>
          loop(currentPos.right, letters :+ letter, steps + 1, dir)
        case ('D', '|' | '-') =>
          loop(currentPos.down, letters, steps + 1, dir)
        case ('D', '+')       =>
          if map.getOrElse(currentPos.right, ' ') != ' ' then
            loop(currentPos.right, letters, steps + 1, 'R')
          else
            loop(currentPos.left, letters, steps + 1, 'L')
        case ('D', letter)    =>
          loop(currentPos.down, letters :+ letter, steps + 1, dir)
        case ('L', '-' | '|') =>
          loop(currentPos.left, letters, steps + 1, dir)
        case ('L', '+')       =>
          if map.getOrElse(currentPos.up, ' ') != ' ' then
            loop(currentPos.up, letters, steps + 1, 'U')
          else
            loop(currentPos.down, letters, steps + 1, 'D')
        case ('L', letter)    =>
          loop(currentPos.left, letters :+ letter, steps + 1, dir)

    loop(findStart, "", 0, 'D')

  def parse(line: String, y: Int): Map[Pos, Char] =
    line.zipWithIndex.collect:
      case (char, x) => Pos(x, y) -> char
    .toMap

  def handleLines(lines: List[String]): Map[Pos, Char] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toMap

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day19
