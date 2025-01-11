package adventofcode2016

import adventofcode2016.Day17.*
import munit.FunSuite

class Day17Suite extends FunSuite:
  test("finds shortest path to the vault"):
    assertEquals(pathToVault("hijkl"), None)
    assertEquals(pathToVault("ihgpwlah"), Some("DDRRRD"))
    assertEquals(pathToVault("kglvqrro"), Some("DDUDRLRRUDRD"))
    assertEquals(pathToVault("ulqzkmiv"), Some("DRURDRUDDLLDLUURRDULRLDUUDDDRR"))

  test("finds shortest path to the vault for the input"):
    assertEquals(pathToVault(importLines()), Some("RDDRLDRURD"))

  test("finds length of longest path to the vault"):
    assertEquals(lengthOfLongestPathToVault("ihgpwlah"), Some(370))
    assertEquals(lengthOfLongestPathToVault("kglvqrro"), Some(492))
    assertEquals(lengthOfLongestPathToVault("ulqzkmiv"), Some(830))

  test("finds length of longest path to the vault for the input"):
    assertEquals(lengthOfLongestPathToVault(importLines()), Some(448))
end Day17Suite
