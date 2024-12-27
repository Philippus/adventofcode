package adventofcode2019

import scala.io.Source
import scala.util.Using

import adventofcode2019.Day06.*
import munit.FunSuite

class Day06Suite extends FunSuite:
  test("sums direct and indirect orbits for the sample"):
    val objects = importSampleLines()
    assertEquals(orbits(objects), 42L)

  test("sums direct and indirect orbits for the input"):
    val objects = importLines()
    assertEquals(orbits(objects), 119831L)

  def importSampleLines(): List[(String, String)] =
    Using.resource(
      Source.fromResource(s"2019/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ):
      _.getLines().toList.map(_.split(')')).flatMap:
        case Array(a, b) => List((a, b))
end Day06Suite
