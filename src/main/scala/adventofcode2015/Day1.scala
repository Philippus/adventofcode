package adventofcode2015

import scala.io.Source
import scala.util.Using

object Day1:
  def findFloor(line: String): Int =
    line.count(_.==('(')) - line.count(_.==(')'))

  def findFloorForInputFile: Int =
    Using.resource(Source.fromResource("2015/day1input.txt")):
      _.getLines().map(findFloor).toSeq.head

  def findFirstPositionInBasement(line: String): Int =
    var currentFloor = 0
    var position = 0
    var passedBasementAtPosition = 0
    line.foreach( char =>
      position += 1
      if char == '(' then currentFloor += 1
      else currentFloor -= 1
      if (currentFloor == -1 && passedBasementAtPosition == 0)
      then
        passedBasementAtPosition = position
    )
    passedBasementAtPosition

  def findPositionForInputFile: Int =
    Using.resource(Source.fromResource("2015/day1input.txt")):
      _.getLines().map(findFirstPositionInBasement).toSeq.head
end Day1
