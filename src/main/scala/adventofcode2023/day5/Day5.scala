package adventofcode2023.day5

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day5:
  case class Interval(start: Long, end: Long)
  def generateMap(lines: Seq[String]): Map[Interval, Long] =
    var map = Map.empty[Interval, Long]
    lines.takeWhile(_.nonEmpty).foreach { line =>
      val (target, source, length) = line match
        case s"$t $s $l" => (t.toLong, s.toLong, l.toLong)
      map = map + (Interval(source, source + length) -> (target - source))
    }
    map

  def lookUp(map: Map[Interval, Long], value: Long): Long =
    val found = map.find(x => x._1.start <= value && value <= x._1.end)
    if found.nonEmpty then
      value + found.get._2
    else
      value

  @tailrec
  def seedToLocation(maps: Seq[Map[Interval, Long]], seed: Long): Long =
    if maps.isEmpty
    then seed
    else seedToLocation(maps.tail, lookUp(maps.head, seed))

  def readInputDocumentPart1: Long =
    Using.resource(Source.fromResource("day5input.txt")): source =>
      val lines                      = source.getLines.toSeq
      val seeds                      = lines.head match
        case s"seeds: $s" => s.trim.split("\\s+").map(_.toLong)
      val (_, seedToSoil)            = lines.splitAt(lines.indexOf("seed-to-soil map:"))
      val seedToSoilMap              = generateMap(seedToSoil.tail)
      val (_, soilToFertilizer)      = lines.splitAt(lines.indexOf("soil-to-fertilizer map:"))
      val soilToFertilizerMap        = generateMap(soilToFertilizer.tail)
      val (_, fertilizertoWater)     = lines.splitAt(lines.indexOf("fertilizer-to-water map:"))
      val fertilizerToWaterMap       = generateMap(fertilizertoWater.tail)
      val (_, waterToLight)          = lines.splitAt(lines.indexOf("water-to-light map:"))
      val waterToLightMap            = generateMap(waterToLight.tail)
      val (_, lightToTemperature)    = lines.splitAt(lines.indexOf("light-to-temperature map:"))
      val lightToTemperatureMap      = generateMap(lightToTemperature.tail)
      val (_, temperatureToHumidity) = lines.splitAt(lines.indexOf("temperature-to-humidity map:"))
      val temperatureToHumidityMap   = generateMap(temperatureToHumidity.tail)
      val (_, humidityToLocation)    = lines.splitAt(lines.indexOf("humidity-to-location map:"))
      val humidityToLocationMap      = generateMap(humidityToLocation.tail)
      seeds.map(seed =>
        seedToLocation(
          Seq(
            seedToSoilMap,
            soilToFertilizerMap,
            fertilizerToWaterMap,
            waterToLightMap,
            lightToTemperatureMap,
            temperatureToHumidityMap,
            humidityToLocationMap
          ),
          seed
        )
      ).min

  def readInputDocumentPart2: Long =
    Using.resource(Source.fromResource("day5input.txt")): source =>
      val lines                      = source.getLines.toSeq
      val seeds                      = lines.head match
        case s"seeds: $s" => s.trim.split("\\s+").map(_.toLong)
      val (_, seedToSoil)            = lines.splitAt(lines.indexOf("seed-to-soil map:"))
      val seedToSoilMap              = generateMap(seedToSoil.tail)
      val (_, soilToFertilizer)      = lines.splitAt(lines.indexOf("soil-to-fertilizer map:"))
      val soilToFertilizerMap        = generateMap(soilToFertilizer.tail)
      val (_, fertilizertoWater)     = lines.splitAt(lines.indexOf("fertilizer-to-water map:"))
      val fertilizerToWaterMap       = generateMap(fertilizertoWater.tail)
      val (_, waterToLight)          = lines.splitAt(lines.indexOf("water-to-light map:"))
      val waterToLightMap            = generateMap(waterToLight.tail)
      val (_, lightToTemperature)    = lines.splitAt(lines.indexOf("light-to-temperature map:"))
      val lightToTemperatureMap      = generateMap(lightToTemperature.tail)
      val (_, temperatureToHumidity) = lines.splitAt(lines.indexOf("temperature-to-humidity map:"))
      val temperatureToHumidityMap   = generateMap(temperatureToHumidity.tail)
      val (_, humidityToLocation)    = lines.splitAt(lines.indexOf("humidity-to-location map:"))
      val humidityToLocationMap      = generateMap(humidityToLocation.tail)

      var min = Long.MaxValue
      seeds.toSeq.grouped(2).toSeq.foreach { group =>
        val start = group.head
        val end   = group.head + group.last
        for (seed <- start to end) {
          val loc = seedToLocation(
            Seq(
              seedToSoilMap,
              soilToFertilizerMap,
              fertilizerToWaterMap,
              waterToLightMap,
              lightToTemperatureMap,
              temperatureToHumidityMap,
              humidityToLocationMap
            ),
            seed
          )
          if (loc < min) min = loc
        }
      }
      min
end Day5
