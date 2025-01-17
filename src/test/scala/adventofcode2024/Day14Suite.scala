package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day14.*
import munit.FunSuite

class Day14Suite extends FunSuite:
  test("draws the tiles for the sample"):
    val str = drawGrid(createGrid(importSampleLines().map(parse), 11, 7))
    println(str)
    /*
    1.12.......
    ...........
    ...........
    ......11.11
    1.1........
    .........1.
    .......1...
     */
    assert(true)

  test("walks the robots for the sample"):
    val robots = importSampleLines().map(parse)
    val str    = drawGrid(createGrid(walkRobots(robots, 11, 7, 100), 11, 7))
    println(str)
    /*
    ......2..1.
    ...........
    1..........
    .11........
    .....1.....
    ...12......
    .1....1....
     */
    assert(true)

  test("calculate safety factor for the sample"):
    val robots = importSampleLines().map(parse)
    val grid   = createGrid(walkRobots(robots, 11, 7, 100), 11, 7)
    assertEquals(calculateSafetyFactor(grid, 11, 7), 12L)

  test("calculate safety factor for the input"):
    val robots = importLines().map(parse)
    val grid   = createGrid(walkRobots(robots, 101, 103, 100), 101, 103)
    assertEquals(calculateSafetyFactor(grid, 101, 103), 221655456L)

  test("calculate safety factor for the input"):
    val robots = importLines().map(parse)
    val grid   = createGrid(walkRobots(robots, 101, 103, 100), 101, 103)
    assertEquals(calculateSafetyFactor(grid, 101, 103), 221655456L)

  test("finds the easter egg"):
    val robots = importLines().map(parse)
    findEasterEgg(robots)
    /*
7858 seconds:

1........11..................1.......................................................................
.................................................................................................1...
.............................................1.......................................................
................1................11..................1....................1..........................
.......1.............................................................................................
........................................................................1............................
.....................................................................................................
.....................................................................................................
.........................................1...........................................................
..............1..............................................1.......................................
.....................................................................................................
...................................................1..................................1..............
...................................................................1.................................
........1...........................................................................1................
.................................1...................................................................
..1.................1................................................................................
.............1..................1.....................................1..............................
...................1......................................................................1..........
................1..................1..............................1..................................
.......1.............................................................................................
.............................................11.....................1..............1.................
............................................................................1........................
.............1.......................................................................................
...................................................1.................................................
.................1...................................................................................
........................................................1............................................
...................1.................................................................................
.................1.................................................................................1.
................................1...........1..................................................1.....
...................1.............................................................1...................
..................................................1.............................1.1.............1....
.....................................................................................................
.............................................................................1.......................
..................................................................1..................................
.....................................................1...............1...............................
1...............................................................................1............1.......
...........................................................................................1.........
..................................................1..........................................1.......
...................1......................................................................1..........
.......................................................................1........................1....
......................1..........................................................................1...
.............................................1.......................................................
..........................................................................................1..........
............1.............................................................................1..........
...................................1111111111111111111111111111111...................................
...................................1.............................1..........1........................
...................................1.............................1...................................
...................................1.............................1...................................
...................................1.............................1...................................
...................................1..............1..............1.....................1.............
...................................1.............111.............1...................................
....1..............................1............11111............1...................................
...................................1...........1111111...........1...................................
..............................1....1..........111111111..........1...................................
...................................1............11111............1...................1...............
................1..................1...........1111111...........1...................................
...................................1..........111111111..........1...................................
....................1..............1.........11111111111.........1...................................
...................................1........1111111111111........1...................................
..1..........1..........1..........1..........111111111..........1......................1............
...................................1.........11111111111.........1...............1...................
...................................1........1111111111111........1...................................
...................................1.......111111111111111.......1..............1....................
...................................1......11111111111111111......1..................1.............1..
...............................1...1........1111111111111........1..1................................
........................1..........1.......111111111111111.......1......1............................
.................1.....1...........1......11111111111111111......1...................................
...................................1.....1111111111111111111.....1......1............................
...............1...................1....111111111111111111111....1..........1........................
.....................1.....1.......1.............111.............1.........................1.........
..........................1........1.............111.............1...................................
...................................1.............111.............1...................................
...................................1.............................1...................................
...................................1.............................1.....................1.............
.....1.............................1.............................1...................................
...................................1.............................1............1........1.............
...................................1111111111111111111111111111111...................................
.....................................................................................................
.....................................................................................................
..................1.........................................1....................................1...
.....................................................................................................
1.................................................................................................1..
.......................................1...............1.............................................
...............................................1.....................................................
.....................................................................................1...............
..........................................11................1........................................
....1................................................................................................
1...1.........................................1...........................1.....1....................
.............................1.....1...........................................1.....................
.1..................................................1............................................1...
...............1...........................................1...........................1.......1.....
.....................................................................................................
.......1.............................................................................................
.....................................................................................................
...........................................1..................................1......................
.....................1.............................................................................1.
.................................................................1...................................
.....................................................................................................
...1..1.....1........................................................................................
.....................................................................................................
............................1........................................................................
......................................................1......................1.......................
.........................................1...........................................................
     */
    assert(true)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day14Suite
