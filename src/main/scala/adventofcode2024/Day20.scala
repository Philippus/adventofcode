package adventofcode2024

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day20:
  case class Pos(x: Int, y: Int):
    def up: Pos    = Pos(x, y - 1)
    def right: Pos = Pos(x + 1, y)
    def down: Pos  = Pos(x, y + 1)
    def left: Pos  = Pos(x - 1, y)
  end Pos

  def picosecondsTroughRacetrack(racetrack: Map[Pos, Char], cheatStart: Pos, cheatEnd: Pos): Long =
    val unvisited: mutable.Map[Pos, Long] = racetrack.filterNot(_._2 == '#').map:
      case (pos, '.') => pos -> Long.MaxValue
      case (pos, 'S') => pos -> 0L
      case (pos, 'E') => pos -> Long.MaxValue
    .to(mutable.Map)

    val visited = mutable.Map[Pos, Long]()
    while !visited.contains(racetrack.find(_._2 == 'E').get._1) do
      val (currentPos, currentVal) = unvisited.minBy(_._2)
      if unvisited.contains(currentPos.up) then
        unvisited.update(currentPos.up, math.min(currentVal + 1, unvisited(currentPos.up)))
      if cheatStart == currentPos.up && cheatEnd == currentPos.up.up && !visited.contains(currentPos.up.up) then
        unvisited.update(currentPos.up, currentVal + 1)
      if unvisited.contains(currentPos.down) then
        unvisited.update(currentPos.down, math.min(currentVal + 1, unvisited(currentPos.down)))
      if cheatStart == currentPos.down && cheatEnd == currentPos.down.down && !visited.contains(currentPos.down.down)
      then
        unvisited.update(currentPos.down, currentVal + 1)
      if unvisited.contains(currentPos.right) then
        unvisited.update(currentPos.right, math.min(currentVal + 1, unvisited(currentPos.right)))
      if cheatStart == currentPos.right && cheatEnd == currentPos.right.right && !visited.contains(
          currentPos.right.right
        )
      then
        unvisited.update(currentPos.right, currentVal + 1)
      if unvisited.contains(currentPos.left) then
        unvisited.update(currentPos.left, math.min(currentVal + 1, unvisited(currentPos.left)))
      if cheatStart == currentPos.left && cheatEnd == currentPos.left.left && !visited.contains(currentPos.left.left)
      then
        unvisited.update(currentPos.left, currentVal + 1)

      unvisited.remove(currentPos)
      visited.update(currentPos, currentVal)
    visited(racetrack.find(_._2 == 'E').get._1)

  def cheatsThatSaveMoreThanNPicoseconds(racetrack: Map[Pos, Char], n: Int): Long =
    val psNoCheats = picosecondsTroughRacetrack(racetrack, Pos(-100, -100), Pos(-200, -200))
    val pairs      =
      for
        x <- 1 until racetrack.maxBy(_._1.x)._1.x
        y <- 1 until racetrack.maxBy(_._1.y)._1.y
        if racetrack(Pos(x, y)) == '#'
      yield Seq(
        if Seq('.', 'E').contains(racetrack(Pos(x, y).up)) then Some(Pos(x, y), Pos(x, y).up) else None,
        if Seq('.', 'E').contains(racetrack(Pos(x, y).down)) then Some(Pos(x, y), Pos(x, y).down) else None,
        if Seq('.', 'E').contains(racetrack(Pos(x, y).right)) then Some(Pos(x, y), Pos(x, y).right) else None,
        if Seq('.', 'E').contains(racetrack(Pos(x, y).left)) then Some(Pos(x, y), Pos(x, y).left) else None
      )
    val cheats     = pairs.flatten.filter(_.isDefined).distinct
    var i          = 0L
    for cheat <- cheats do
      if psNoCheats - picosecondsTroughRacetrack(racetrack, cheat.get._1, cheat.get._2) >= n then i += 1
    i

  def parse(line: String, y: Int): Map[Pos, Char] =
    line.zipWithIndex.collect:
      case (char, x) => Pos(x, y) -> char
    .toMap

  def handleLines(lines: List[String]): Map[Pos, Char] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toMap

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day20
