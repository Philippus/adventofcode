package adventofcode2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day08:
  def parse(line: String, wide: Int, tall: Int): Seq[String] =
    line.grouped(wide * tall).toSeq

  def countDigit(layer: String, digit: Char): Int =
    layer.count(_.==(digit))

  def calculateLayer(layers: Seq[String]): Int =
    val fewestZero = layers.minBy(layer => countDigit(layer, '0'))
    countDigit(fewestZero, '1') * countDigit(fewestZero, '2')

  @tailrec
  def findPixels(layers: Seq[String], acc: String = ""): String =
    if layers.exists(_.isEmpty) then
      acc
    else
      val data  = layers.map(_.head).mkString
      val pixel = (data.indexOf('0'), data.indexOf('1')) match
        case (-1, _)         => '1'
        case (_, -1)         => '0'
        case (a, b) if a < b => '0'
        case _               => '1'
      findPixels(layers.map(_.tail), acc :+ pixel)

  def drawImage(pixels: String): Unit =
    pixels.map:
      case '0' => ' '
      case '1' => '#'
    .grouped(25).foreach(println)

  def importLines(): String =
    Using.resource(Source.fromResource(s"2019/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq.head
end Day08
