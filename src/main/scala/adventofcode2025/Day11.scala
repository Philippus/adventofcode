package adventofcode2025

import scala.io.Source
import scala.util.Using

object Day11:
  case class Device(id: String, outputs: List[String])

  def countPathsFromYouToOut(devices: List[Device]): Long =
    val queue = scala.collection.mutable.ListBuffer[Device](devices.find(_.id == "you").get)
    var paths = 0L
    while queue.nonEmpty do
      val current                    = queue.head
      if current.outputs.contains("out") then paths += 1L
      val devicesToAdd: List[Device] = devices.filter(d => current.outputs.filterNot(_.==("out")).contains(d.id))
      queue.addAll(devicesToAdd)
      queue.remove(0)
    paths

  def countPathsFromSrcToDestWithMemoization(devices: List[Device], src: String, dest: String): Long =
    val memo                             = scala.collection.mutable.Map[String, Long]()
    def findPaths(current: String): Long =
      if current == dest then
        1L
      else if current == "out" then
        0L
      else
        val neighbours = devices.find(_.id == current).get.outputs
        val paths      = memo.getOrElseUpdate(current, neighbours.map(findPaths).sum)
        paths

    findPaths(src)

  def countPathsFromSrcToDestWithMemoizationVisitingFftAndDac(devices: List[Device]): Long =
    math.max(
      countPathsFromSrcToDestWithMemoization(devices, "svr", "fft") *
        countPathsFromSrcToDestWithMemoization(devices, "fft", "dac") *
        countPathsFromSrcToDestWithMemoization(devices, "dac", "out"),
      countPathsFromSrcToDestWithMemoization(devices, "svr", "dac") *
        countPathsFromSrcToDestWithMemoization(devices, "dac", "fft") *
        countPathsFromSrcToDestWithMemoization(devices, "fft", "out")
    )

  def parse(input: String): List[Device] =
    input.split('\n').toList.map:
      case s"$src: $output" =>
        Device(src, output.split(" ").toList)
    :+ Device("out", List.empty[String])

  def importLines(): String =
    Using.resource(Source.fromResource("2025/day11input.txt")): source =>
      source.mkString
end Day11
