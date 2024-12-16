package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day14:
  case class Robot(px: Int, py: Int, vx: Int, vy: Int)

  @tailrec
  def walkRobots(robots: Seq[Robot], width: Int, height: Int, secondsRemaining: Int): Seq[Robot] =
    if secondsRemaining == 0 then
      robots
    else
      val newRobots = robots.map(robot =>
        robot.copy(
          px = (((robot.px + robot.vx) % width) + width)   % width,
          py = (((robot.py + robot.vy) % height) + height) % height
        )
      )
      walkRobots(newRobots, width, height, secondsRemaining - 1)

  def calculateSafetyFactor(grid: Seq[((Int, Int), Int)], width: Int, height: Int): Long =
    val q1 =
      (for
        x <- 0 until width / 2
        y <- 0 until height / 2
      yield grid.find(_._1 == (x, y)).map(_._2).get).sum
    val q2 =
      (for
        x <- width / 2 + 1 until width
        y <- 0 until height / 2
      yield grid.find(_._1 == (x, y)).map(_._2).get).sum

    val q3 =
      (for
        x <- 0 until width / 2
        y <- height / 2 + 1 until height
      yield grid.find(_._1 == (x, y)).map(_._2).get).sum

    val q4 =
      (for
        x <- width / 2 + 1 until width
        y <- height / 2 + 1 until height
      yield grid.find(_._1 == (x, y)).map(_._2).get).sum

    q1 * q2 * q3 * q4

  def findEasterEgg(robots: Seq[Robot]): Unit =
    for
      i <- 0 to 10000
    do
      val newRobots = walkRobots(robots, 101, 103, i)
      if newRobots.groupBy(_.py).map(_._2.length).max > 15 && newRobots.groupBy(_.px).map(_._2.length).max > 15 then
        val str = drawGrid(createGrid(newRobots, 101, 103))
        println(s"$i seconds:\n" + str)

  def createGrid(robots: Seq[Robot], width: Int, height: Int): Seq[((Int, Int), Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield ((x, y), robots.count(robot => x == robot.px && y == robot.py))

  def drawGrid(grid: Seq[((Int, Int), Int)]): String =
    grid.map {
      case ((0, _), x) => if (x == 0) "\n." else s"\n$x"
      case ((_, _), x) => if (x == 0) "." else s"$x"
    }.mkString

  def parse(line: String): Robot =
    line match
      case s"p=$px,$py v=$vx,$vy" =>
        Robot(px.toInt, py.toInt, vx.toInt, vy.toInt)

  def handleLines(lines: List[String]): List[Robot] =
    lines.map(parse)

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day14
