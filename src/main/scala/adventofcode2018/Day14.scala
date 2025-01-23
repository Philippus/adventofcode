package adventofcode2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day14:
  def scoreNextTenRecipes(nextTenAfter: Int): String =
    @tailrec
    def loop(elves: List[Int], scores: Vector[Int]): String =
      if scores.length >= (nextTenAfter + 10) then
        scores.slice(nextTenAfter, nextTenAfter + 10).mkString("")
      else
        val newScores = scores ++ elves.map(elf => scores(elf)).sum.toString.map(_.asDigit)
        val newElves  = elves.map: elf =>
          (elf + 1 + scores(elf)) % newScores.length
        loop(newElves, newScores)
    loop(List(0, 1), Vector(3, 7))

  def createRecipesP2(slice: String): Int =
    val sliceDigits                                      = slice.map(_.asDigit)
    @tailrec
    def loop(elves: List[Int], scores: Vector[Int]): Int =
      if scores.length > 90000000 then
        scores.indexOfSlice(sliceDigits)
      else
        val newScores = scores ++ elves.map(elf => scores(elf)).sum.toString.map(_.asDigit)
        val newElves  = elves.map: elf =>
          (elf + 1 + scores(elf)) % newScores.length
        loop(newElves, newScores)

    loop(List(0, 1), Vector(3, 7))

  def importLines(): Int =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next().toInt
end Day14
