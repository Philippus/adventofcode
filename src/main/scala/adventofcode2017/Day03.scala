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

    /*
    Part two solved in spreadsheet:

    363010	349975	330785	312453	295229	279138	266330	130654
    6591	6444	6155	5733	5336	5022	2450	128204
    13486	147   	142	    133	    122	    59	    2391	123363
    14267	304	    5	    4	    2	    57	    2275	116247
    15252	330	    10	    1	    1	    54	    2105	109476
    16295	351	    11	    23	    25	    26	    1968	103128
    17008	362	    747	    806	    880	    931	    957	    98098
    17370	35487	37402	39835	42452	45220	47108	48065
     */
end Day03
