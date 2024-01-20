package adventofcode2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day07:
  def hasAbba(s: String): Boolean =
    val slidings          = s.sliding(2)
    val slidingsWithIndex = slidings.zipWithIndex.toSeq
    val evens             = slidingsWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val odds              = slidingsWithIndex.filter(_._2 % 2 == 1).map(_._1)
    evens.sliding(2).exists(slide => slide.head == slide.last.reverse && slide.head.head != slide.head.last) ||
    odds.sliding(2).exists(slide => slide.head == slide.last.reverse && slide.head.head != slide.head.last)

  def supportsTls(s: String): Boolean =
    val splits       = s.split("\\[|\\]").zipWithIndex.toSeq
    val hypernets    = splits.filter(_._2 % 2 == 1).map(_._1)
    val nonHypernets = splits.filter(_._2 % 2 == 0).map(_._1)
    nonHypernets.exists(hasAbba) && !hypernets.exists(hasAbba)

  def getAbas(s: String): Seq[String] =
    val slidings = s.sliding(3)
    slidings.filter(slide => slide.head == slide.last && slide(1) != slide.head).toSeq

  def supportsSsl(s: String): Boolean =
    val splits       = s.split("\\[|\\]").zipWithIndex.toSeq
    val hypernets    = splits.filter(_._2 % 2 == 1).map(_._1)
    val nonHypernets = splits.filter(_._2 % 2 == 0).map(_._1)
    val abas         = nonHypernets.flatMap(getAbas)
    abas.map(s => s(1).toString ++ s.head.toString ++ s(1).toString).intersect(hypernets.flatMap(getAbas)).nonEmpty

  def readInputfile(): Seq[String] =
    Using.resource(Source.fromResource("2016/day07input.txt")): source =>
      source.getLines().toSeq
end Day07
