package adventofcode2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09:
  def insideGarbage(str: String, level: Int): Int =
    str match
      case s">,<$rest" => 0 + insideGarbage(rest, level)
      case s">,{$rest" => 0 + insideGroup(rest, level)
      case s">$rest"   => 0 + insideGroup(rest, level - 1)
      case _           => insideGarbage(str.tail, level)

  def insideGroup(str: String, level: Int): Int =
    str match
      case s"}"        => level
      case s"},{$rest" => level + insideGroup(rest, level)
      case s"},<$rest" => level + insideGarbage(rest, level)
      case s"}$rest"   => level + insideGroup(rest, level - 1)
      case s"{$rest"   => insideGroup(rest, level + 1)
      case s"<$rest"   => insideGarbage(rest, level + 1)

  def handleString(str: String): Int =
    str.replaceAll("!.", "") match
      case s"{$rest" => insideGroup(rest, 1)

  def insideGarbagePt2(str: String): Int =
    str match
      case s">,<$rest" => 3 + insideGarbagePt2(rest)
      case s">,{$rest" => 3 + insideGroupPt2(rest)
      case s">$rest"   => 1 + insideGroupPt2(rest)
      case _           => insideGarbagePt2(str.tail)

  def insideGroupPt2(str: String): Int =
    str match
      case s"}"        => 1
      case s"},{$rest" => 3 + insideGroupPt2(rest)
      case s"},<$rest" => 3 + insideGarbagePt2(rest)
      case s"}$rest"   => 1 + insideGroupPt2(rest)
      case s"{$rest"   => 1 + insideGroupPt2(rest)
      case s"<$rest"   => 1 + insideGarbagePt2(rest)

  def handleStringPt2(str: String): Int =
    val s          = str.replaceAll("!.", "")
    val notGarbage = s match
      case s"{$rest" => 1 + insideGroupPt2(rest)
    s.length - notGarbage

  def readInputFile(): String =
    Using.resource(Source.fromResource("2017/day09input.txt")):
      _.getLines().toSeq.head
end Day09
