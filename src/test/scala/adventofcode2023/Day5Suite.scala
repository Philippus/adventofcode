package adventofcode2023

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.io.Source
import scala.util.Using

import adventofcode2023.day5.Day5.*
import munit.FunSuite

class Day5Suite extends FunSuite:
  test("generate map works as expected"):
    assertEquals(generateMap(Seq("50 98 2")), Map(Interval(98L, 100L) -> -48L))

  test("look up works as expected"):
    assertEquals(lookUp(Map(Interval(98L, 100L) -> 50L), 99L), 149L)
    assertEquals(lookUp(Map(Interval(98L, 100L) -> 50L), 1L), 1L)
    assertEquals(lookUp(Map(Interval(0L, 30L) -> 20L), 5L), 25L)

  test("part 1 works for the sample input"):
    def readInputDocument: Long =
      Using.resource(Source.fromResource("day5sampleinput.txt")): source =>
        val lines = source.getLines.toSeq
        val seeds = lines.head match
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
        val (_, humidityToLocation)   = lines.splitAt(lines.indexOf("humidity-to-location map:"))
        val humidityToLocationMap     = generateMap(humidityToLocation.tail)
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
    assertEquals(readInputDocument, 35L)

  test("part 2 works for the sample input"):
    def readInputDocument: Long =
      Using.resource(Source.fromResource("day5sampleinput.txt")): source =>
        val lines                      = source.getLines.toSeq
        val seeds                      = lines.head match
          case s"seeds: $s" => s.trim.split("\\s+")
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
        val (_, humidityToLocation)   = lines.splitAt(lines.indexOf("humidity-to-location map:"))
        val humidityToLocationMap     = generateMap(humidityToLocation.tail)

        var min = Long.MaxValue
        seeds.toSeq.grouped(2).toSeq.foreach { group =>
          val start = group.head.toLong
          val end   = group.head.toLong + group.last.toLong
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
    assertEquals(readInputDocument, 46L)

  test("part 1 works"):
    assertEquals(readInputDocumentPart1, 175622908L)

// this test takes loooong
//  test("part 2 works"):
//    assertEquals(readInputDocumentPart2, 5200543L)

end Day5Suite
