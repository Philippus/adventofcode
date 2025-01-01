package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day20:
  case class Position(x: Int, y: Int, z: Int):
    val manhattanDistance: Int = math.abs(x) + math.abs(y) + math.abs(z)
  end Position

  case class Velocity(x: Int, y: Int, z: Int)
  case class Acceleration(x: Int, y: Int, z: Int)
  case class Particle(position: Position, velocity: Velocity, acceleration: Acceleration):
    def tick(): Particle =
      val newVelocity = Velocity(velocity.x + acceleration.x, velocity.y + acceleration.y, velocity.z + acceleration.z)
      val newPosition = Position(position.x + newVelocity.x, position.y + newVelocity.y, position.z + newVelocity.z)
      Particle(position = newPosition, velocity = newVelocity, acceleration = acceleration)
  end Particle

  def particleClosestToOrigin(particles: Seq[Particle]): Int =
    @tailrec
    def loop(particles: Seq[(Particle, Int)], tick: Int): Int =
      if tick > 1000 then
        particles.minBy(_._1.position.manhattanDistance)._2
      else
        loop(particles.map(p => (p._1.tick(), p._2)), tick + 1)

    loop(particles.zipWithIndex, 0)

  def particlesLeftAfterCollisionsAreResolved(particles: Seq[Particle]): Int =
    @tailrec
    def loop(particles: Seq[(Particle, Int)], tick: Int): Int =
      if tick > 1000 then
        particles.length
      else
        val newParticles         = particles.map(p => (p._1.tick(), p._2))
        val filteredNewParticles = newParticles.groupBy(_._1.position).filterNot(_._2.size > 1).values.flatten.toSeq
        loop(filteredNewParticles, tick + 1)

    loop(particles.zipWithIndex, 0)

  def handleLines(lines: Seq[String]): Seq[Particle] =
    lines.map:
      case s"p=<$px,$py,$pz>, v=<$vx,$vy,$vz>, a=<$ax,$ay,$az>" =>
        Particle(
          Position(px.toInt, py.toInt, pz.toInt),
          Velocity(vx.toInt, vy.toInt, vz.toInt),
          Acceleration(ax.toInt, ay.toInt, az.toInt)
        )

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day20
