package adventofcode2021

import scala.io.Source
import scala.util.Using

object Day08:
  def count1478(s: Seq[Seq[String]]): Int =
    s.map(_.count(x => Seq(2, 3, 4, 7).contains(x.length))).sum

  def handleLine(s: String): Seq[String] =
    s.split('|').last.trim.split(' ')

  def determineNumbers(patterns: Seq[String]) =
    val eight = patterns.find(_.length == 7).get
    val one   = patterns.find(_.length == 2).get
    val seven = patterns.find(_.length == 3).get
    val four  = patterns.find(_.length == 4).get
    val six   = patterns.filter(_.length == 6).find(seven.diff(_).length == 1).get
    val zero  = patterns.filter(p => p.length == 6 && p != six).find(four.diff(_).length == 1).get
    val nine  = patterns.find(p => p.length == 6 && p != six && p != zero).get
    val a     = seven.diff(one)
    val c     = seven.diff(six)
    val d     = four.diff(zero)
    val e     = zero.diff(nine)
    val f     = one.intersect(six)
    val b     = four.replace(c, "").replace(d, "").replace(f, "")
    val g     = zero.replace(a, "").replace(b, "").replace(c, "").replace(d, "").replace(e, "").replace(f, "")
    val two   = patterns.find(p => p.sorted == (a ++ c ++ d ++ e ++ g).sorted).get
    val three = patterns.find(p => p.sorted == (a ++ c ++ d ++ f ++ g).sorted).get
    val five  = patterns.find(p => p.sorted == (a ++ b ++ d ++ f ++ g).sorted).get

    Map(
      0 -> zero,
      1 -> one,
      2 -> two,
      3 -> three,
      4 -> four,
      5 -> five,
      6 -> six,
      7 -> seven,
      8 -> eight,
      9 -> nine
    )

  def outputValue(patterns: Seq[String], output: Seq[String]): Int =
    val numbers = determineNumbers(patterns)
    output.map(o => numbers.find(_._2.sorted == o.sorted).get._1.toString).mkString.toInt

  def outputValues(values: Seq[(Seq[String], Seq[String])]): Int =
    values.map((p, o) => outputValue(p, o)).sum

  def handleLinePartTwo(s: String): (Seq[String], Seq[String]) =
    s match
      case s"$patterns | $output" =>
        (patterns.split(' ').toSeq, output.split(' ').toSeq)

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2021/day08input.txt")): source =>
      source.getLines().toSeq
end Day08
