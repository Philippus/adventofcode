package adventofcode2018

import scala.concurrent.duration.Duration

import adventofcode2018.Day14.*
import munit.FunSuite

class Day14Suite extends FunSuite:
  override val munitTimeout = Duration(200, "s")
  test("scores the ten recipes after n recipes for the sample"):
    assertEquals(scoreNextTenRecipes(5), "0124515891")
    assertEquals(scoreNextTenRecipes(9), "5158916779")
    assertEquals(scoreNextTenRecipes(18), "9251071085")
    assertEquals(scoreNextTenRecipes(2018), "5941429882")

  test("scores the ten recipes after n recipes for the input"):
    assertEquals(scoreNextTenRecipes(importLines()), "6126491027")

  test("calculates number of recipes before score sequence for the sample"):
    assertEquals(createRecipesP2("51589"), 9)
    assertEquals(createRecipesP2("01245"), 5)
    assertEquals(createRecipesP2("92510"), 18)
    assertEquals(createRecipesP2("59414"), 2018)

  test("calculates number of recipes before score sequence for the input"):
    assertEquals(createRecipesP2(importLines().toString), 20191616)
end Day14Suite
