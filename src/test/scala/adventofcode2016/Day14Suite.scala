package adventofcode2016

import scala.concurrent.duration.Duration

import adventofcode2016.Day14.*
import munit.FunSuite

class Day14Suite extends FunSuite:
  override val munitTimeout = Duration(1000, "s")
  test("generates hashes"):
    assertEquals(md5("abc18"), "0034e0923cc38887a57bd7b1d4f953df")
    assertEquals(md5("abc39"), "347dac6ee8eeea4652c7476d0f97bee5")
    assertEquals(md5("abc816"), "3aeeeee1367614f3061d165a5fe3cac3")

  test("determines triples"):
    assertEquals(hasTriple("0034e0923cc38887a57bd7b1d4f953df"), Some('8'))
    assertEquals(hasTriple("347dac6ee8eeea4652c7476d0f97bee5"), Some('e'))

  test("determines quintuples"):
    assert(hasQuintuple("3aeeeee1367614f3061d165a5fe3cac3", 'e'))

  test("determines index of 64th key"):
    assertEquals(determineIndexOf64thKey("abc"), 22728)
    assertEquals(determineIndexOf64thKey(importLines()), 25427)

  test("determines stretched hash"):
    assertEquals(stretchedHash("abc0"), "a107ff634856bb300138cac6568c0f24")
    assertEquals(stretchedHash("abc10"), "4a81e578d9f43511ab693eee1a75f194")
    assertEquals(stretchedHash("abc89"), "eaa5c17bec47565b98275b404eeeeea6")
    assertEquals(stretchedHash("abc22551"), "2df6e9378c3c53abed6d3508b6285fff")
    assertEquals(stretchedHash("abc22859"), "2e559978fffff9ac9c9012eb764c6391")

  test("determines index of 64th key with key stretching"):
    assertEquals(determineIndexOf64thKey("abc", stretched = true), 22551)
    assertEquals(determineIndexOf64thKey(importLines(), stretched = true), 22045)
end Day14Suite
