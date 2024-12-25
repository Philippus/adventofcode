package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day25.*
import munit.FunSuite

class Day25Suite extends FunSuite:
  test("determines heights for the sample input"):
    val (locks, keys) = handleLines(importSampleLines())
    assertEquals(locks.map(heights), Seq(Seq(0, 5, 3, 4, 3), Seq(1, 2, 0, 5, 3)))
    assertEquals(keys.map(heights), Seq(Seq(5, 0, 2, 1, 3), Seq(4, 3, 4, 0, 2), Seq(3, 0, 2, 0, 1)))

  test("determines overlaps for the sample input"):
    assert(overlaps(Seq(0, 5, 3, 4, 3), Seq(5, 0, 2, 1, 3)))
    assert(overlaps(Seq(0, 5, 3, 4, 3), Seq(4, 3, 4, 0, 2)))
    assert(!overlaps(Seq(0, 5, 3, 4, 3), Seq(3, 0, 2, 0, 1)))
    assert(overlaps(Seq(1, 2, 0, 5, 3), Seq(5, 0, 2, 1, 3)))
    assert(!overlaps(Seq(1, 2, 0, 5, 3), Seq(4, 3, 4, 0, 2)))
    assert(!overlaps(Seq(1, 2, 0, 5, 3), Seq(3, 0, 2, 0, 1)))

  test("sums all unique lock/key pairs that fit together without overlapping for the sample input"):
    val (locks, keys) = handleLines(importSampleLines())
    assertEquals(sumNonOverlappingLockKeyPairs(locks, keys), 3)

  test("sums all unique lock/key pairs that fit together without overlapping for the input"):
    val (locks, keys) = handleLines(importLines())
    assertEquals(sumNonOverlappingLockKeyPairs(locks, keys), 3356)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day25Suite
