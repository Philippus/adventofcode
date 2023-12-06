package adventofcode2015

import scala.io.Source
import scala.util.Using

import adventofcode2015.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("test signal"):
    assertEquals(runConnection(Seq("123 -> x")), Set(Wire("x", 123)))

  test("small diagram"):
    assertEquals(runConnection(Seq("123 -> x", "456 -> y", "x AND y -> d")).find(_.id == "d").get, Wire("d", 72))

  test("simple circuit"):
    Using.resource(Source.fromResource("2015/day7sampleinput.txt")): source =>
      val lines = source.getLines().toSeq
      val h     = runConnection(lines).find(_.id == "h").get.signal
      assertEquals(((h % 65536) + 65536) % 65536, 65412)

  test("calculates signal on wire a for input file"):
    assertEquals(runCircuit, 3176)

  test("calculates signal on wire a after rewiring in part two"):
    assertEquals(runCircuitAfterRewiring, 14710)
end Day07Suite
