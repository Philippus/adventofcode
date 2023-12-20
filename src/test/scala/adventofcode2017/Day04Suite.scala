package adventofcode2017

import adventofcode2017.Day04.*
import munit.FunSuite

class Day04Suite extends FunSuite:
  test("validates passphrase"):
    assert(validate("aa bb cc dd ee"))
    assert(!validate("aa bb cc dd aa"))
    assert(validate("aa bb cc dd aaa"))
    assertEquals(readInputFile.map(validate).count(x => x), 386)

  test("validates passphrase according to new policy"):
    assert(validateWithNewPolicy("abcde fghij"))
    assert(!validateWithNewPolicy("abcde xyz ecdab"))
    assert(validateWithNewPolicy("a ab abc abd abf abj"))
    assert(validateWithNewPolicy("iiii oiii ooii oooi oooo"))
    assert(!validateWithNewPolicy("oiii ioii iioi iiio"))
    assertEquals(readInputFile.map(validateWithNewPolicy).count(x => x), 386)
end Day04Suite
