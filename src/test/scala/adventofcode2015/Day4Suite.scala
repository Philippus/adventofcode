package adventofcode2015

import adventofcode2015.Day4.*
import munit.FunSuite

class Day4Suite extends FunSuite:
  test("md5 returns the correct hex string"):
    assert(md5("abcdef609043").startsWith("00000"))

  test("determines the secret key"):
    assertEquals(determineSecretKey("abcdef", 5), 609043)
    assertEquals(determineSecretKey("pqrstuv", 5), 1048970)

  test("determines the secret key for the input string with five zeroes"):
    assertEquals(determineSecretKey("yzbqklnj", 5), 282749)

  test("determines the secret key for the input string with six zeroes"):
    assertEquals(determineSecretKey("yzbqklnj", 6), 9962624)
end Day4Suite
