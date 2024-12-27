package adventofcode2019

import scala.io.Source
import scala.util.Using

import adventofcode2019.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("determines max thruster signal for the samples"):
    assertEquals(maxThrusterSignal(List(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0)), 43210)
    assertEquals(
      maxThrusterSignal(List(3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99,
        0, 0)),
      54321
    )
    assertEquals(
      maxThrusterSignal(List(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
        1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0)),
      65210
    )

  test("determines max thruster signal for the input"):
    val input = importLines()
    assertEquals(maxThrusterSignal(input), 116680)

  def importSampleLines(): List[Int] =
    Using.resource(
      Source.fromResource(s"2019/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList.head.split(',').toList.map(_.toInt)
end Day07Suite
