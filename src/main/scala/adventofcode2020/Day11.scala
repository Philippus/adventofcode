package adventofcode2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11:
  case class Seat(x: Int, y: Int, char: Char)

  def seatPassengers(seats: Vector[Seat]): Int =
    def occupiedAdjacent(seat: Seat, seats: Vector[Seat]): Int =
      (for
        x <- (seat.x - 1) to (seat.x + 1)
        y <- (seat.y - 1) to (seat.y + 1)
        if !(seat.x == x && seat.y == y)
      yield seats.count(s => s.x == x && s.y == y && s.char == '#')).sum

    @tailrec
    def loop(seats: Vector[Seat]): Int =
      val newSeats =
        seats.map:
          case seat if seat.char == 'L' && occupiedAdjacent(seat, seats) == 0 => seat.copy(char = '#')
          case seat if seat.char == '#' && occupiedAdjacent(seat, seats) >= 4 => seat.copy(char = 'L')
          case seat                                                           => seat
      if newSeats == seats then
        seats.count(_.char == '#')
      else
        loop(newSeats)
    loop(seats)

  def seatPassengersWithVisibility(seats: Vector[Seat]): Int =
    def withinBounds(x: Int, y: Int) =
      x >= 0 && x <= 93 && y >= 0 && y <= 93

    def seesAdjacent(seat: Seat, seats: Vector[Seat]): Int =
      val x = seat.x
      val y = seat.y

      var count = 0

      // left
      var leftX = seat.x - 1
      var done  = false
      while !done && withinBounds(leftX, y) do
        seats.find(s => s.x == leftX && s.y == y) match
          case None                           =>
            leftX -= 1
          case Some(seat) if seat.char == '#' =>
            count += 1
            done = true
          case _                              =>
            done = true

      // right
      var rightX = seat.x + 1
      done = false
      while !done && withinBounds(rightX, y) do
        seats.find(s => s.x == rightX && s.y == y) match
          case None                           =>
            rightX += 1
          case Some(seat) if seat.char == '#' =>
            count += 1
            done = true
          case _                              =>
            done = true

      // up
      var upY = seat.y - 1
      done = false
      while !done && withinBounds(x, upY) do
        seats.find(s => s.x == x && s.y == upY) match
          case None                           =>
            upY -= 1
          case Some(seat) if seat.char == '#' =>
            count += 1
            done = true
          case _                              =>
            done = true

      // down
      var downY = seat.y + 1
      done = false
      while !done && withinBounds(x, downY) do
        seats.find(s => s.x == x && s.y == downY) match
          case None                           =>
            downY += 1
          case Some(seat) if seat.char == '#' =>
            count += 1
            done = true
          case _                              =>
            done = true

      // upleft
      var leftX2 = seat.x - 1
      var upY2   = seat.y - 1
      done = false
      while !done && withinBounds(leftX2, upY2) do
        seats.find(s => s.x == leftX2 && s.y == upY2) match
          case None                           =>
            leftX2 -= 1
            upY2 -= 1
          case Some(seat) if seat.char == '#' =>
            count += 1
            done = true
          case _                              =>
            done = true

      // upRight
      var rightX2 = seat.x + 1
      var upY3    = seat.y - 1
      done = false
      while !done && withinBounds(rightX2, upY3) do
        seats.find(s => s.x == rightX2 && s.y == upY3) match
          case None                           =>
            rightX2 += 1
            upY3 -= 1
          case Some(seat) if seat.char == '#' =>
            count += 1
            done = true
          case _                              =>
            done = true

      // downLeft
      var leftX3 = seat.x - 1
      var downY2 = seat.y + 1
      done = false
      while !done && withinBounds(leftX3, downY2) do
        seats.find(s => s.x == leftX3 && s.y == downY2) match
          case None                           =>
            leftX3 -= 1
            downY2 += 1
          case Some(seat) if seat.char == '#' =>
            count += 1
            done = true
          case _                              =>
            done = true

      // downRight
      var rightX3 = seat.x + 1
      var downY3  = seat.y + 1
      done = false
      while !done && withinBounds(rightX3, downY3) do
        seats.find(s => s.x == rightX3 && s.y == downY3) match
          case None                           =>
            rightX3 += 1
            downY3 += 1
          case Some(seat) if seat.char == '#' =>
            count += 1
            done = true
          case _                              =>
            done = true

      count

    @tailrec
    def loop(seats: Vector[Seat]): Int =
      val newSeats =
        seats.map:
          case seat if seat.char == 'L' && seesAdjacent(seat, seats) == 0 => seat.copy(char = '#')
          case seat if seat.char == '#' && seesAdjacent(seat, seats) >= 5 => seat.copy(char = 'L')
          case seat                                                       => seat
      if newSeats == seats then
        seats.count(_.char == '#')
      else
        loop(newSeats)

    loop(seats)

  def createGrid(width: Int, height: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield (x, y)

  def drawGrid(seats: Vector[Seat], width: Int, height: Int): String =
    val grid = createGrid(width, height)
    createGrid(width, height).map:
      case (0, y) => if seats.contains(Seat(0, y, 'L')) then "\nL"
        else "\n."
      case (x, y) => if seats.contains(Seat(x, y, 'L')) then "L"
        else "."
    .mkString

  def parse(line: String, y: Int): Vector[Seat]      =
    line.zipWithIndex.collect:
      case (char, x) if char == 'L' => Seat(x, y, 'L')
    .toVector
  def handleLines(lines: List[String]): Vector[Seat] =
    lines.zipWithIndex.flatMap(l => parse(l._1, l._2))
      .toVector

  def importLines(): List[String] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toList
end Day11
