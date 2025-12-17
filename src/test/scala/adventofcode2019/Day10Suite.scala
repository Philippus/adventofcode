package adventofcode2019

import scala.io.Source
import scala.util.Using

import adventofcode2019.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("finds best location for monitoring station for the sample"):
    val input = importSampleLines()
    assertEquals(findBestPosition(parse(input)), Pos(3, 4))

  test("finds max detected asteroids for the sample"):
    val input = importSampleLines()
    assertEquals(findMaxDetectedAsteroids(parse(input)), 8)

  test("finds best location for monitoring station for the 2nd sample"):
    val input = importSample2Lines()
    assertEquals(findBestPosition(parse(input)), Pos(8, 3))

  test("finds max detected asteroids for the 2nd sample"):
    val input = importSample2Lines()
    assertEquals(findMaxDetectedAsteroids(parse(input)), 30)

  test("finds best location for monitoring station for the 3rd sample"):
    val input = importSample3Lines()
    assertEquals(findBestPosition(parse(input)), Pos(11, 13))

  test("finds max detected asteroids for the 3rd sample"):
    val input = importSample3Lines()
    assertEquals(findMaxDetectedAsteroids(parse(input)), 210)

  test("finds best location for monitoring station for the input"):
    val input = importLines()
    assertEquals(findBestPosition(parse(input)), Pos(11, 13))

  test("finds max detected asteroids for the 3rd sample"):
    val input = importLines()
    assertEquals(findMaxDetectedAsteroids(parse(input)), 227)

  test("finds nth asteroid to be vaporized for the 3rd sample"):
    val input = importSample3Lines()
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 1), Pos(11, 12))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 2), Pos(12, 1))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 3), Pos(12, 2))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 10), Pos(12, 8))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 20), Pos(16, 0))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 50), Pos(16, 9))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 100), Pos(10, 16))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 199), Pos(9, 6))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 200), Pos(8, 2))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 201), Pos(10, 9))
    assertEquals(nthVaporizedAsteroid(findBestPosition(parse(input)), parse(input), 299), Pos(11, 1))

  test("finds 200th asteroid to be vaporized for the 3rd sample"):
    val input = importSample3Lines()
    assertEquals(twoHundredthVaporizedAsteroid(parse(input)), 802)

  test("finds 200th asteroid to be vaporized for the input"):
    val input = importLines()
    assertEquals(twoHundredthVaporizedAsteroid(parse(input)), 604)

  def importSample3Lines(): String =
    Using.resource(Source.fromResource("2019/day10sample3input.txt")): source =>
      source.mkString

  def importSample2Lines(): String =
    Using.resource(Source.fromResource("2019/day10sample2input.txt")): source =>
      source.mkString

  def importSampleLines(): String =
    Using.resource(Source.fromResource("2019/day10sampleinput.txt")): source =>
      source.mkString
end Day10Suite
