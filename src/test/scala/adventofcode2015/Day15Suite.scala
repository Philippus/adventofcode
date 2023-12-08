package adventofcode2015

import adventofcode2015.Day15.*
import munit.FunSuite

class Day15Suite extends FunSuite:
  test("scores a mixture"):
    val ingredients = Map(
      Ingredient("Botterscotch", -1, -2, 6, 3, 8) -> 44,
      Ingredient("Cinnamon", 2, 3, -2, -1, 3)     -> 56
    )
    assertEquals(scoreMixture(ingredients)._1, 62842880)

  test("finds best score for example"):
    val ingredients = Seq(
      Ingredient("Botterscotch", -1, -2, 6, 3, 8),
      Ingredient("Cinnamon", 2, 3, -2, -1, 3)
    )
    assertEquals(scoreCookieWithTwoIngredients(ingredients), 62842880)

  test("finds best score for input file"):
    assertEquals(scoreCookieForInputFile, 222870)

  test("finds best score with calorie limit for input file"):
    assertEquals(scoreCookieWithCalorieLimitForInputFile, 117936)
end Day15Suite
