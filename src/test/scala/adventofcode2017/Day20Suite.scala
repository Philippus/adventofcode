package adventofcode2017

import scala.io.Source
import scala.util.Using

import adventofcode2017.Day20.*
import munit.FunSuite

class Day20Suite extends FunSuite:
  test("determines particle in the long term closest to position <0,0,0> for the sample"):
    val particles = handleLines(importSampleLines())
    assertEquals(particleClosestToOrigin(particles), 0)

  test("determines particle in the long term closest to position <0,0,0> for the input"):
    val particles = handleLines(importLines())
    assertEquals(particleClosestToOrigin(particles), 344)

  test("determines particle in the long term closest to position <0,0,0> for the sample"):
    val particles =
      Seq(
        Particle(Position(-6, 0, 0), Velocity(3, 0, 0), Acceleration(0, 0, 0)),
        Particle(Position(-4, 0, 0), Velocity(2, 0, 0), Acceleration(0, 0, 0)),
        Particle(Position(-2, 0, 0), Velocity(1, 0, 0), Acceleration(0, 0, 0)),
        Particle(Position(3, 0, 0), Velocity(-1, 0, 0), Acceleration(0, 0, 0))
      )
    assertEquals(particlesLeftAfterCollisionsAreResolved(particles), 1)
  
  test("determines particle in the long term closest to position <0,0,0> for the input"):
    val particles = handleLines(importLines())
    assertEquals(particlesLeftAfterCollisionsAreResolved(particles), 404)

  def importSampleLines(): Seq[String] =
    Using.resource(
      Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toSeq
end Day20Suite
