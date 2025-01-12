package adventofcode2017

import scala.io.Source
import scala.util.Using

import adventofcode2017.Day13.*
import munit.FunSuite

class Day13Suite extends FunSuite:
  test("calculates severity of trip through firewall for the sample"):
    val firewallLayers = handleLines(importSampleLines())
    assertEquals(severity(firewallLayers), (true, 24))

  test("calculates severity of trip through firewall for the input"):
    val firewallLayers = handleLines(importLines())
    assertEquals(severity(firewallLayers), (true, 1476))

  test("calculates delay of trip to not get caught for the sample"):
    val firewallLayers = handleLines(importSampleLines())
    assertEquals(calculateDelayToNotGetCaught(firewallLayers), 10)

  test("calculates delay of trip to not get caught for the input"):
    val firewallLayers = handleLines(importLines())
    assertEquals(calculateDelayToNotGetCaught(firewallLayers), 3937334)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2017/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day13Suite
