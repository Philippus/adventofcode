package adventofcode2015

import adventofcode2015.Day5.*
import munit.FunSuite

class Day5Suite extends FunSuite:
  test("determines niceness of strings"):
    assert(isNice("ugknbfddgicrmopn"))
    assert(isNice("aaa"))
    assert(isNaughty("jchzalrnumimnmhp"))
    assert(isNaughty("dvszwmarrgswjxmb"))

  test("calculates amount of nice strings in input file"):
    assertEquals(determineNiceStringsForFile(isNice), 255)

  test("determines niceness of strings in part two"):
    assert(isNicePartTwo("qjhvhtzxzqqjkmpb"))
    assert(isNicePartTwo("xxyxx"))
    assert(isNaughtyPartTwo("uurcxstgmygtbstg"))
    assert(isNaughtyPartTwo("ieodomkazucvgmuy"))

  test("calculates amount of nice strings in input file for part two"):
    assertEquals(determineNiceStringsForFile(isNicePartTwo), 55)
end Day5Suite
