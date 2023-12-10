package adventofcode2015

import adventofcode2015.Day19.*
import munit.FunSuite

class Day19Suite extends FunSuite:
  test("replaces in molecule"):
    val replacements = Seq(
      "H" -> "HO",
      "H" -> "OH",
      "O" -> "HH"
    )
    val result = replacements.flatMap(x => replace("HOH", x)).distinct
    assertEquals(result.length, 4)

  test("counts steps to molecule"):
    val replacements = Seq(
      "e" -> "H",
      "e" -> "O",
      "H" -> "HO",
      "H" -> "OH",
      "O" -> "HH"
    )
      assertEquals(stepsToMedicine("HOH", replacements), 3)
      assertEquals(stepsToMedicine("HOHOHO", replacements), 6)

  test("counts distinct molecules for the input file"):
    assertEquals(countDistinctMoleculesForFile(), 518)
end Day19Suite
