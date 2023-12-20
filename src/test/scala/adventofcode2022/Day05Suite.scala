package adventofcode2022

import adventofcode2022.Day05.*
import munit.FunSuite

class Day05Suite extends FunSuite:
  test("works for the example"):
    val stacks       = Set(
      Stack(1, Seq("N", "Z")),
      Stack(2, Seq("D", "C", "M")),
      Stack(3, Seq("P"))
    )
    val instructions = Seq(
      Instruction(1, 2, 1),
      Instruction(3, 1, 3),
      Instruction(2, 2, 1),
      Instruction(1, 1, 2)
    )
    val newStacks    = followInstructions(stacks, instructions)
    assertEquals(newStacks.toSeq.sortBy(_.id).map(_.crates.head).mkString, "CMZ")
    val newStacks2   = followInstructions(stacks, instructions, true)
    assertEquals(newStacks2.toSeq.sortBy(_.id).map(_.crates.head).mkString, "MCD")

  test("works for the input file"):
    val stacks       = Set(
      Stack(1, Seq("V", "C", "W", "L", "R", "M", "F", "Q")),
      Stack(2, Seq("L", "Q", "D")),
      Stack(3, Seq("B", "N", "C", "W", "G", "R", "S", "P")),
      Stack(4, Seq("G", "Q", "B", "H", "D", "C", "L")),
      Stack(5, Seq("S", "Z", "F", "L", "G", "V")),
      Stack(6, Seq("P", "N", "G", "D")),
      Stack(7, Seq("W", "C", "F", "V", "P", "Z", "D")),
      Stack(8, Seq("S", "M", "D", "P", "C")),
      Stack(9, Seq("C", "P", "M", "V", "T", "W", "N", "Z"))
    )
    val instructions = importLines()

    val newStacks               = followInstructions(stacks, instructions)
    assertEquals(newStacks.toSeq.sortBy(_.id).map(_.crates.head).mkString, "VWLCWGSDQ")
    val newStacksCratemover9001 = followInstructions(stacks, instructions, true)
    assertEquals(newStacksCratemover9001.toSeq.sortBy(_.id).map(_.crates.head).mkString, "TCGLQSLPW")
end Day05Suite
