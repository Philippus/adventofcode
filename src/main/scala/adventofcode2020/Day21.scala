package adventofcode2020

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day21:
  def countSafeIngredients(foods: Vector[(Vector[String], Vector[String])]): Int =
    val allergens         = foods.flatMap(_._2).distinct
    val unsafeIngredients = allergens.map: allergen =>
      val candidatesForAllergen = foods.filter(_._2.contains(allergen)).map(_._1)
      candidatesForAllergen.reduce((l1, l2) => l1.intersect(l2))
    .flatten
    foods.flatMap(_._1).count(!unsafeIngredients.contains(_))

  def dangerousIngredientsList(foods: Vector[(Vector[String], Vector[String])]): String =
    val allergens         = foods.flatMap(_._2).distinct
    val unsafeIngredients = allergens.map: allergen =>
      val candidatesForAllergen = foods.filter(_._2.contains(allergen)).map(_._1)
      (allergen, candidatesForAllergen.reduce((l1, l2) => l1.intersect(l2)))

    val allergensToIngredients: mutable.Map[String, String] = mutable.Map.empty[String, String]
    while allergens.length != allergensToIngredients.size do
      unsafeIngredients.foreach:
        case (a, b) if (a, b.diff(allergensToIngredients.values.toSeq))._2.length == 1 =>
          allergensToIngredients.update(a, b.diff(allergensToIngredients.values.toSeq).head)
        case _                                                                         =>
          ()
    allergensToIngredients.toVector.sortBy(_._1).map(_._2).mkString(",")

  def handleLines(lines: Vector[String]): Vector[(Vector[String], Vector[String])] =
    lines.map:
      case s"$ingredients (contains $allergens)" =>
        (ingredients.split(' ').toVector, allergens.split(", ").toVector)

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2020/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day21
