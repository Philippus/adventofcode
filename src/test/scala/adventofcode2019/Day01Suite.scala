package adventofcode2019

import adventofcode2019.Day01.*
import munit.FunSuite

class Day01Suite extends FunSuite:
  test("determines fuel requirement"):
    assertEquals(fuelRequired(12), 2)
    assertEquals(fuelRequired(14), 2)
    assertEquals(fuelRequired(1969), 654)
    assertEquals(fuelRequired(100756), 33583)

  test("determines fuel requirement for the input"):
    val masses = readInputFile()
    assertEquals(masses.map(fuelRequired).sum, 3414791)

  test("determines fuel requirement part two"):
    assertEquals(fuelRequiredIncludingFuel(12), 2)
    assertEquals(fuelRequiredIncludingFuel(1969), 966)
    assertEquals(fuelRequiredIncludingFuel(100756), 50346)

  test("determines fuel requirement for the input part two"):
    val masses = readInputFile()
    assertEquals(masses.map(fuelRequiredIncludingFuel).sum, 5119312)
end Day01Suite
