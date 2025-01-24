package adventofcode2020

import scala.io.Source
import scala.util.Using

import adventofcode2020.Day21.*
import munit.FunSuite

class Day21Suite extends FunSuite:
  test("counts safe ingredients for the sample"):
    val foods = handleLines(importSampleLines())
    assertEquals(countSafeIngredients(foods), 5)

  test("counts safe ingredients for the input"):
    val foods = handleLines(importLines())
    assertEquals(countSafeIngredients(foods), 2659)

  test("creates dangerous ingredient list for the sample"):
    val foods = handleLines(importSampleLines())
    assertEquals(dangerousIngredientsList(foods), "mxmxvkd,sqjhc,fvjkl")

  test("creates dangerous ingredient list for the input"):
    val foods = handleLines(importLines())
    assertEquals(dangerousIngredientsList(foods), "rcqb,cltx,nrl,qjvvcvz,tsqpn,xhnk,tfqsb,zqzmzl")

  def importSampleLines(): Vector[String] =
    Using.resource(
      Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toVector
end Day21Suite
