package adventofcode2017

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day21:
  val ruleBookMap: mutable.Map[String, String] = mutable.Map[String, String]()

  def rotate(s: String): String =
    s.split('/').toSeq.transpose.map(_.reverse.mkString).mkString("/")

  def flip(s: String): String =
    s.split('/').map(_.reverse.mkString).mkString("/")

  def applyRule(ruleBook: Map[String, String], pattern: String): String =
    ruleBookMap.getOrElseUpdate(
      pattern, {
        val variants = Seq(
          pattern,
          rotate(pattern),
          rotate(rotate(pattern)),
          rotate(rotate(rotate(pattern))),
          flip(pattern),
          rotate(flip(pattern)),
          rotate(rotate(flip(pattern))),
          rotate(rotate(rotate(flip(pattern))))
        )

        ruleBook(variants.find(ruleBook.contains).get)
      }
    )

  def parse(line: String, y: Int, shiftX: Int = 0, shiftY: Int = 0): Map[(Int, Int), Char] =
    line.zipWithIndex.collect:
      case (char, x) => (x + shiftX, y + shiftY) -> char
    .toMap

  def patternToMap(pattern: String, shiftX: Int = 0, shiftY: Int = 0): Map[(Int, Int), Char] =
    pattern.split("\n").toList.zipWithIndex.flatMap(l => parse(l._1, l._2, shiftX, shiftY))
      .toMap

  def squareToPattern(square: Seq[(Int, Int, Char)]): String =
    val size = square.maxBy(_._2)._2 - square.minBy(_._2)._2 + 1
    val s    =
      for
        y <- square.minBy(_._2)._2 to square.maxBy(_._2)._2
        x <- square.minBy(_._1)._1 to square.maxBy(_._1)._1
      yield square.find(s => s._1 == x && s._2 == y).get._3
    s.mkString("").grouped(size).mkString("/")

  def applyRules(ruleBook: Map[String, String], pattern: String): String =
    val size       = pattern.split("\n").length
    val (width, l) = if size % 2 == 0 then
      (size / 2, 2)
    else
      (size / 3, 3)
    val map        = patternToMap(pattern)
    val squares    =
      for
        j <- 0 until width // 0..2
        i <- 0 until width // 0..2
      yield (for
        k <- 0 until l // 0..1
        m <- 0 until l // 0..1
      yield (i * l + k, j * l + m, map(i * l + k, j * l + m)))

    val x = squares.grouped(width).zipWithIndex.toList.flatMap: (r, i) =>
      r.zipWithIndex.flatMap: (f, j) =>
        patternToMap(applyRule(ruleBook, squareToPattern(f)).replace("/", "\n"), j * (l + 1), i * (l + 1)).map:
          case ((x, y), c) => (x, y, c)

    squareToPattern(x).replace("/", "\n")

  def pixelsOnAfterIterations(ruleBook: Map[String, String], n: Int, initialPattern: String): Int =
    @tailrec
    def loop(iteration: Int, pattern: String): String =
      if iteration == n then
        pattern
      else
        loop(iteration + 1, applyRules(ruleBook, pattern))
    loop(0, initialPattern).count(_.==('#'))

  def handleLines(lines: Seq[String]): Map[String, String] =
    lines.map:
      case s"$from => $to" => from -> to
    .toMap

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day21
