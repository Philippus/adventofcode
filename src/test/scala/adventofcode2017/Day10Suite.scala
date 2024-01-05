package adventofcode2017

import adventofcode2017.Day10.*
import munit.FunSuite

class Day10Suite extends FunSuite:
  test("processes lengths for the example"):
    assertEquals(processLengths(0.to(4), Seq(3, 4, 1, 5)), 12)

  test("processes lengths for the input file"):
    assertEquals(processLengths(0.to(255), readInputFile()), 13760)

  test("reduces sparse hash"):
    assertEquals(reduceSparseHash(Seq(65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22)), Seq(64))

  test("numbers to hex"):
    assertEquals(numbersToHex(Seq(64, 7, 255)), "4007ff")

  test("string to lengths and suffix"):
    assertEquals(stringToLengthsAndSuffix(""), suffix)

  test("calculates knot hashes"):
    assertEquals(processLengthsAsAscii(0.to(255), stringToLengthsAndSuffix("")), "a2582a3a0e66e6e86e3812dcb672a272")
    assertEquals(
      processLengthsAsAscii(0.to(255), stringToLengthsAndSuffix("AoC 2017")),
      "33efeb34ea91902bb2f59c9920caa6cd"
    )
    assertEquals(
      processLengthsAsAscii(0.to(255), stringToLengthsAndSuffix("1,2,3")),
      "3efbe78a8d82f29979031a4aa0b16a9d"
    )
    assertEquals(
      processLengthsAsAscii(0.to(255), stringToLengthsAndSuffix("1,2,4")),
      "63960835bcdc130f0b66d7ff4f6a5a8e"
    )

  test("calculates knot hash for the input file"):
    val input = readInputFileAsAscii() ++ suffix

    assertEquals(processLengthsAsAscii(0.to(255), input), "2da93395f1a6bb3472203252e3b17fe5")
end Day10Suite
