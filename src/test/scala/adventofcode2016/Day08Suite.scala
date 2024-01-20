package adventofcode2016

import adventofcode2016.Day08.*
import munit.FunSuite

class Day08Suite extends FunSuite:
  test("updates screen"):
    val s = followInstructions(Seq("rect 3x2", "rotate column x=1 by 1", "rotate row y=0 by 4"), 7, 3)
    println(s.map(x => x.map(c => if c then "#" else ".").mkString).mkString("\n"))

  test("counts lit pixels on screen"):
    val instructions = readInputfile()
    val s            = followInstructions(instructions, 50, 6)
    println(s.map(x => x.map(c => if c then "#" else " ").mkString).mkString("\n")) // EOARGPHYAO
    assertEquals(s.map(r => r.count(_.==(true))).sum, 128)
end Day08Suite
