package adventofcode2024

import scala.io.Source
import scala.util.Using

import adventofcode2024.Day23.*
import munit.FunSuite

class Day23Suite extends FunSuite:
  test("finds all sets of three interconnected computers with a computer starting with T for the sample"):
    val (computers, connections) = handleLines(importSampleLines())
    assertEquals(interconnectedComputers(computers, connections, 3).length, 12)

  test("sums sets of three interconnected computers with a computer starting with T for the sample"):
    val (computers, connections) = handleLines(importSampleLines())
    assertEquals(sumInterconnectedComputersWithT(computers, connections), 7)

  test("sums interconnected computers with a computer starting with T for the input"):
    val (computers, connections) = handleLines(importLines())
    assertEquals(sumInterconnectedComputersWithT(computers, connections), 1314)

  test("finds max possible computers in set for the sample"):
    val (computers, connections) = handleLines(importSampleLines())
    assertEquals(maxPossibleComputersInSet(computers, connections), 5)

  test("finds max possible computers in set for the input"):
    val (computers, connections) = handleLines(importLines())
    assertEquals(maxPossibleComputersInSet(computers, connections), 14)

  test("finds password for the LAN party for the sample"):
    val (computers, connections) = handleLines(importSampleLines())
    assertEquals(findPasswordForLANParty(computers, connections), "co,de,ka,ta")

  test("finds password for the LAN party for the input"):
    val (computers, connections) = handleLines(importLines())
    assertEquals(findPasswordForLANParty(computers, connections), "bg,bu,ce,ga,hw,jw,nf,nt,ox,tj,uu,vk,wp")

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day23Suite
