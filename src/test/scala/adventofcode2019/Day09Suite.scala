package adventofcode2019

import scala.io.Source
import scala.util.Using

import adventofcode2019.Day09.*
import munit.FunSuite

class Day09Suite extends FunSuite:
  test("works for the samples"):
    val instructionsForProgram1 =
      List(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99).map(_.toLong)
    val program1                = instructionsForProgram1.zipWithIndex.map: (a, i) =>
      i.toLong -> a
    .toMap
    assertEquals(process(program1, 0, Seq(0)), instructionsForProgram1)

    val program2 = List(1102, 34915192, 34915192, 7, 4, 7, 99, 0).zipWithIndex.map: (a, i) =>
      i.toLong -> a.toLong
    .toMap
    assertEquals(process(program2, 0, Seq(0)), Seq(1219070632396864L))

    val program3 = List(104L, 1125899906842624L, 99L).zipWithIndex.map: (a, i) =>
      i.toLong -> a
    .toMap
    assertEquals(process(program3, 0, Seq(0)), Seq(1125899906842624L))

  test("returns the BOOST keycode"):
    val program = importLines()
    assertEquals(process(program, 0, Seq(1L)), Seq(2457252183L))

  test("find the coordinates of the distress signal"):
    val program = importLines()
    assertEquals(process(program, 0, Seq(2L)), Seq(70634L))

  def importSampleLines(): Map[Long, Long] =
    Using.resource(
      Source.fromResource(s"2019/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList.head.split(',').zipWithIndex.map: (a, i) =>
        i.toLong -> a.toLong
      .toMap
end Day09Suite
