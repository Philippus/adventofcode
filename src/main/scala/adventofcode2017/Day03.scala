package adventofcode2017

import scala.annotation.tailrec

object Day03:
  case class Spiral(end: Int, side: Int)

  @tailrec
  def findSpiral(i: Int, currentSpiral: Spiral): Spiral =
    if currentSpiral.end + currentSpiral.side * 4 - 4 < i then
      findSpiral(
        i,
        Spiral(currentSpiral.end + currentSpiral.side * 4 + 4, currentSpiral.side + 2)
      )
    else
      currentSpiral

  def manhattanDistance(i: Int, spiral: Spiral): Int =
    val bottom = spiral.end - spiral.side / 2
    val left   = spiral.end - spiral.side + 1 - spiral.side / 2
    val top    = spiral.end - spiral.side - spiral.side + 2 - spiral.side / 2
    val right  = spiral.end - spiral.side - spiral.side - spiral.side + 3 - spiral.side / 2

    math.min(
      math.min(
        math.min(
          math.abs(bottom - i),
          math.abs(left - i)
        ),
        math.abs(top - i)
      ),
      math.abs(right - i)
    ) + spiral.side / 2
end Day03
