package adventofcode2015

import scala.io.Source
import scala.util.Using

import spray.json._

object Day12:
  def sumNumbers(jsValue: JsValue, ignoreRed: Boolean = false): Int = jsValue match
    case JsNumber(value)                                                               =>
      value.toInt
    case JsArray(elems)                                                                =>
      elems.map(sumNumbers(_, ignoreRed)).sum
    case JsObject(fields) if ignoreRed && !fields.values.exists(_.==(JsString("red"))) =>
      fields.values.map(sumNumbers(_, ignoreRed)).sum
    case JsObject(fields) if !ignoreRed                                                =>
      fields.values.map(sumNumbers(_, ignoreRed)).sum
    case _                                                                             =>
      0

  def readInputFile(): String =
    Using.resource(Source.fromResource("2015/day12input.txt")):
      _.getLines().toSeq.head
end Day12
