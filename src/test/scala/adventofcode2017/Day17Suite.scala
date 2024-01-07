package adventofcode2017

import scala.collection.mutable.ArrayBuffer

import adventofcode2017.Day17.*
import munit.FunSuite

class Day17Suite extends FunSuite:
  test("value after n insertions"):
    assertEquals(insert(3, 1), ArrayBuffer(0, 1))
    assertEquals(insert(3, 2), ArrayBuffer(0, 2, 1))
    assertEquals(insert(3, 3), ArrayBuffer(0, 2, 3, 1))
    assertEquals(insert(3, 4), ArrayBuffer(0, 2, 4, 3, 1))
    assertEquals(insert(3, 5), ArrayBuffer(0, 5, 2, 4, 3, 1))
    assertEquals(insert(3, 6), ArrayBuffer(0, 5, 2, 4, 3, 6, 1))
    assertEquals(insert(3, 7), ArrayBuffer(0, 5, 7, 2, 4, 3, 6, 1))
    assertEquals(insert(3, 8), ArrayBuffer(0, 5, 7, 2, 4, 3, 8, 6, 1))
    assertEquals(insert(3, 9), ArrayBuffer(0, 9, 5, 7, 2, 4, 3, 8, 6, 1))
    val after2017 = insert(3, 2017)
    assertEquals(after2017.length, 2018)
    assertEquals(after2017(after2017.indexOf(2017) + 1), 638)

    val afterInsertionForInput = insert(328, 2017)
    assertEquals(afterInsertionForInput(afterInsertionForInput.indexOf(2017) + 1), 1670)

  test("value after zero"):
    assertEquals(lastValueAfterZero(3, 15), 12)
    assertEquals(lastValueAfterZero(3, 16), 16)
    assertEquals(lastValueAfterZero(328, 50_000_000), 2316253)
end Day17Suite
