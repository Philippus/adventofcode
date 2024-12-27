package adventofcode2019

import scala.io.Source
import scala.util.Using

import adventofcode2019.Day05.*
import munit.FunSuite

class Day05Suite extends FunSuite:
  test("Intcode supports opcodes 3 and 4"):
    assertEquals(process(List(3, 0, 4, 0, 99), 0, 5), 5)

  test("Intcode supports parameter mode"):
    val lines = List(1002, 4, 3, 4, 33)
    assertEquals(process(lines, 0, 1), -1)

  test("produces correct diagnostic code for the input"):
    val lines = importLines()
    assertEquals(process(lines, 0, 1), 9431221)

  test("Intcode supports opcodes 5-8"):
    assertEquals(process(List(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8), 0, 8), 1)
    assertEquals(process(List(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8), 0, 8), 0)
    assertEquals(process(List(3, 3, 1108, -1, 8, 3, 4, 3, 99), 0, 8), 1)
    assertEquals(process(List(3, 3, 1107, -1, 8, 3, 4, 3, 99), 0, 7), 1)

  test("Intcode supports opcodes 5-8 in a larger example"):
    val program = List(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
      1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
      999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99
    )
    assertEquals(process(program, 0, 7), 999)
    assertEquals(process(program, 0, 8), 1000)
    assertEquals(process(program, 0, 9), 1001)

  test("produces correct diagnostic code for system ID 5"):
    val lines = importLines()
    assertEquals(process(lines, 0, 5), 1409363)

  def importSampleLines(): List[Int] =
    Using.resource(
      Source.fromResource(s"2019/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList.head.split(',').toList.map(_.toInt)
end Day05Suite
