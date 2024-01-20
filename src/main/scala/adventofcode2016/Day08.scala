package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day08:
  def followInstructions(instructions: Seq[String], width: Int, height: Int): Vector[Vector[Boolean]] =
    def loop(instructions: Seq[String], screen: Vector[Vector[Boolean]]): Vector[Vector[Boolean]] =
      if instructions.isEmpty then
        screen
      else
        var newScreen = screen
        instructions.head match
          case s"rect ${a}x${b}"           =>
            for
              ia <- 0.until(a.toInt)
              ib <- 0.until(b.toInt)
            yield newScreen = newScreen.updated(ib, newScreen(ib).updated(ia, true))
            loop(instructions.tail, newScreen)
          case s"rotate row y=$a by $b"    =>
            val row        = newScreen(a.toInt)
            val updatedRow = row.takeRight(b.toInt) ++ row.take(width - b.toInt)
            newScreen = newScreen.updated(a.toInt, updatedRow)
            loop(instructions.tail, newScreen)
          case s"rotate column x=$a by $b" =>
            var transposedScreen = screen.transpose
            val row              = transposedScreen(a.toInt)
            val updatedRow       = row.takeRight(b.toInt) ++ row.take(height - b.toInt)
            transposedScreen = transposedScreen.updated(a.toInt, updatedRow)
            loop(instructions.tail, transposedScreen.transpose)
    loop(instructions, Vector.fill(height, width)(false))

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day08input.txt")): source =>
      source.getLines().toSeq
end Day08
