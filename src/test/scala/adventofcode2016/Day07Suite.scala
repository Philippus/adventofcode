package adventofcode2016

import adventofcode2016.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("checks for ABBA"):
    assert(hasAbba("abba"))
    assert(!hasAbba("aaaa"))
    assert(hasAbba("ioxxoj"))

  test("checks for TLS support"):
    assert(supportsTls("abba[mnop]qrst"))
    assert(!supportsTls("abcd[bddb]xyyx"))
    assert(!supportsTls("aaaa[qwer]tyui"))
    assert(supportsTls("ioxxoj[asdfgh]zxcvbn"))

  test("counts IP Addresses with TLS support"):
    val candidates = readInputfile()
    assertEquals(candidates.map(supportsTls).count(_.==(true)), 118)

  test("gets abas"):
    assertEquals(getAbas("aba"), Seq("aba"))
    assertEquals(getAbas("bab"), Seq("bab"))

  test("checks for SSL support"):
    assert(supportsSsl("aba[bab]xyz"))
    assert(!supportsSsl("xyx[xyx]xyx"))
    assert(supportsSsl("aaa[kek]eke"))
    assert(supportsSsl("zazbz[bzb]cdb"))

  test("counts IP Addresses with SSL support"):
    val candidates = readInputfile()
    assertEquals(candidates.map(supportsSsl).count(_.==(true)), 260)
end Day07Suite
