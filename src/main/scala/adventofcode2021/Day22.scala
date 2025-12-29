package adventofcode2021

import scala.io.Source
import scala.util.Using

object Day22:
  case class RebootStep(on: Boolean, xRange: (Int, Int), yRange: (Int, Int), zRange: (Int, Int))

  def executeRebootStepsInSmallRegion(rebootSteps: List[RebootStep]): Int =
    val region = scala.collection.mutable.Map[(Int, Int, Int), Boolean]()

    for
      rebootStep <- rebootSteps
      if rebootStep.xRange._1 >= -50 && rebootStep.xRange._2 <= 50 && rebootStep.yRange._1 >= -50 && rebootStep.yRange
        ._2 <= 50 && rebootStep.zRange._1 >= -50 && rebootStep.zRange._2 <= 50
      x          <- rebootStep.xRange._1.to(rebootStep.xRange._2)
      y          <- rebootStep.yRange._1.to(rebootStep.yRange._2)
      z          <- rebootStep.zRange._1.to(rebootStep.zRange._2)
    do
      region((x, y, z)) = rebootStep.on

    region.count(_._2 == true)

  def parse(input: String): List[RebootStep] =
    input.split('\n').toList.map:
      case s"on x=$xFrom..$xTo,y=$yFrom..$yTo,z=$zFrom..$zTo"  =>
        RebootStep(true, (xFrom.toInt, xTo.toInt), (yFrom.toInt, yTo.toInt), (zFrom.toInt, zTo.toInt))
      case s"off x=$xFrom..$xTo,y=$yFrom..$yTo,z=$zFrom..$zTo" =>
        RebootStep(false, (xFrom.toInt, xTo.toInt), (yFrom.toInt, yTo.toInt), (zFrom.toInt, zTo.toInt))

  def importLines(): String =
    Using.resource(Source.fromResource("2021/day22input.txt")): source =>
      source.mkString
end Day22
