package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day05:
  @tailrec
  def followInstructions(
      instructions: Seq[Int],
      currentPosition: Int = 0,
      steps: Int = 0,
      partTwo: Boolean = false
  ): Int =
    if currentPosition >= instructions.length then
      steps
    else
      val offset = instructions(currentPosition)
      followInstructions(
        instructions.updated(
          currentPosition,
          instructions(currentPosition) + (if partTwo && offset >= 3 then -1 else 1)
        ),
        currentPosition + instructions(currentPosition),
        steps + 1,
        partTwo
      )

  def followInstructionsUsingArray(instructions: Seq[Int], partTwo: Boolean = false): Int =
    var position = 0
    val iAsArray = instructions.toArray
    var steps    = 0
    while position < instructions.length do
      val offset = iAsArray(position)
      iAsArray(position) = iAsArray(position) + (if partTwo && offset >= 3 then -1 else 1)
      steps += 1
      position = position + offset
    steps

  def readInputFile: Seq[Int] =
    Using.resource(Source.fromResource("2017/day05input.txt")):
      _.getLines().map(_.toInt).toSeq
end Day05
