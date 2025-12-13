package adventofcode2020

import adventofcode2020.Day25.*
import munit.FunSuite

class Day25Suite extends FunSuite:
  test("finds loop size for the sample"):
    assertEquals(findLoopSize(5764801L), 8L)
    assertEquals(findLoopSize(17807724L), 11L)

  test("finds encryption key for the sample"):
    assertEquals(findEncryptionKey(17807724L, findLoopSize(5764801L)), 14897079L)
    assertEquals(findEncryptionKey(5764801L, findLoopSize(17807724L)), 14897079L)

  test("finds loop size and encryption key for the input"):
    val keys = parse(importLines())
    assertEquals(findEncryptionKey(keys.last, findLoopSize(keys.head)), 354320L)
    assertEquals(findEncryptionKey(keys.head, findLoopSize(keys.last)), 354320L)
end Day25Suite
