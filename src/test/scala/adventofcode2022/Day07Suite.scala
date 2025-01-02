package adventofcode2022

import scala.io.Source
import scala.util.Using

import adventofcode2022.Day07.*
import adventofcode2022.Day07.FileOrDir.Root
import munit.FunSuite

class Day07Suite extends FunSuite:
  test("sums size of directories of max 100000 for the sample"):
    val commands = importSampleLines()
    val contents = handleCommands(commands)
    assertEquals(sumSizeOfDirectoriesOfMax100000(contents), 95437L)

  test("sums size of directories of max 100000 for the input"):
    val commands = importLines()
    val contents = handleCommands(commands)
    assertEquals(sumSizeOfDirectoriesOfMax100000(contents), 1141028L)

  test("finds size of directory to delete for the sample"):
    val commands = importSampleLines()
    val contents = handleCommands(commands)
    assertEquals(sizeOfDirectoryToDelete(contents), 24933642L)

  test("finds size of directory to delete for the sample"):
    val commands = importLines()
    val contents = handleCommands(commands)
    assertEquals(sizeOfDirectoryToDelete(contents), 8278005L)

  def importSampleLines(): List[String] =
    Using.resource(
      Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("suite", "")}sampleinput.txt")
    ): source =>
      source.getLines().toList
end Day07Suite
