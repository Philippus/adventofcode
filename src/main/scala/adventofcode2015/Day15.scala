package adventofcode2015

import scala.io.Source
import scala.math.max
import scala.util.Using

object Day15:
  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  def scoreMixture(ingredientsToTableSpoons: Map[Ingredient, Int]): (Int, Int) =
    val capacity   = ingredientsToTableSpoons.map:
      case (ingredient, tablespoons) =>
        ingredient.capacity * tablespoons
    .sum
    val durability = ingredientsToTableSpoons.map:
      case (ingredient, tablespoons) =>
        ingredient.durability * tablespoons
    .sum
    val flavor     = ingredientsToTableSpoons.map:
      case (ingredient, tablespoons) =>
        ingredient.flavor * tablespoons
    .sum
    val texture    = ingredientsToTableSpoons.map:
      case (ingredient, tablespoons) =>
        ingredient.texture * tablespoons
    .sum
    val calories   = ingredientsToTableSpoons.map:
      case (ingredient, tablespoons) =>
        ingredient.calories * tablespoons
    .sum
    (max(capacity, 0) * max(durability, 0) * max(flavor, 0) * max(texture, 0), calories)

  def scoreCookieWithTwoIngredients(ingredients: Seq[Ingredient], requiredCalories: Option[Int] = None): Int =
    val teaspoonss        =
      for
        i <- 1.to(100)
      yield Seq(i, 100 - i)
    val mixtures          = teaspoonss.map(teaspoons => ingredients.zip(teaspoons))
    val scoresAndCalories = mixtures.map(mixture => scoreMixture(mixture.toSeq.toMap))
    requiredCalories match
      case Some(value) => scoresAndCalories.filter(_._2 == value).map(_._1).max
      case None        => scoresAndCalories.map(_._1).max

  def scoreCookie(ingredients: Seq[Ingredient], requiredCalories: Option[Int] = None): Int =
    val teaspoonss        =
      for
        i <- 1.to(100)
        j <- 1.to(100)
        k <- 1.to(100)
      yield Seq(i, j, k, 100 - i - j - k)
    val mixtures          = teaspoonss.map(teaspoons => ingredients.zip(teaspoons))
    val scoresAndCalories = mixtures.map(mixture => scoreMixture(mixture.toSeq.toMap))
    requiredCalories match
      case Some(value) => scoresAndCalories.filter(_._2 == value).map(_._1).max
      case None        => scoresAndCalories.map(_._1).max

  def handleLine(line: String): Ingredient =
    line match
      case s"$name: capacity $capacity, durability $durability, flavor $flavor, texture $texture, calories $calories" =>
        Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)

  def scoreCookieForInputFile: Int =
    Using.resource(Source.fromResource("2015/day15input.txt")): source =>
      val ingredients = source.getLines().map(handleLine).toList
      scoreCookie(ingredients)

  def scoreCookieWithCalorieLimitForInputFile: Int =
    Using.resource(Source.fromResource("2015/day15input.txt")): source =>
      val ingredients = source.getLines().map(handleLine).toList
      scoreCookie(ingredients, Some(500))
end Day15
