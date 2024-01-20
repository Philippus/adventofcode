package adventofcode2016

import adventofcode2016.Day04.*
import munit.FunSuite

class Day04Suite extends FunSuite:
  test("finds sector id's of real rooms"):
    val candidates = readInputfile()
    val sum        = candidates.map(sectorIdOfRealRoom).sum
    assertEquals(sum, 158835)

  test("decrypts room names"):
    assertEquals(decryptEncryptedRoomName("qzmt-zixmtkozy-ivhz-343[abcde]"), "very encrypted name")

  test("finds sector ID of the northpole object storage"):
    val candidates             = readInputfile()
    val northPoleObjectStorage =
      candidates.filter(c => sectorIdOfRealRoom(c) != 0).find(r =>
        decryptEncryptedRoomName(r) == "northpole object storage"
      )
    assertEquals(sectorIdOfRealRoom(northPoleObjectStorage.get), 993)
end Day04Suite
