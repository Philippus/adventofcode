package adventofcode2023

import java.lang.System.arraycopy
import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11:
  case class Pos(y: Int, x: Int)

  def generateCombinations(galaxies: Seq[Pos]): Seq[(Pos, Pos)] =
    @tailrec
    def helper(galaxies: Seq[Pos], acc: Seq[(Pos, Pos)]): Seq[(Pos, Pos)] =
      if galaxies.length < 2 then
        acc
      else
        helper(galaxies.tail, acc ++ galaxies.tail.map(g => (galaxies.head, g)))
    helper(galaxies, Seq.empty)

  def calculateDistance(galaxies: Seq[(Pos, Pos)]): Int =
    galaxies.map((a, b) => (a.x - b.x).abs + (a.y - b.y).abs).sum

  def findGalaxies(grid: Array[Array[Char]]): Seq[Pos] =
    (for
      x <- grid.indices
      y <- grid.head.indices
      if grid(x)(y) == ('#')
    yield Pos(x, y))

  def transposeMatrix(matrix: Array[Array[Char]]): Array[Array[Char]] =
    var newMatrix: Array[Array[Char]] = Array.fill(matrix.head.length)(Array.fill(matrix.length)('.'))
    (for
      x <- matrix.head.indices
      y <- matrix.indices
    yield newMatrix(x)(y) = matrix(y)(x))
    newMatrix

  private def insertRow(
      originalData: Array[Array[Char]],
      numberOfRowWhereToInsertData: Int,
      dataToInsert: Array[Char]
  ) = {
    val output = new Array[Array[Char]](originalData.length + 1)
    if (numberOfRowWhereToInsertData >= 0) arraycopy(originalData, 0, output, 0, numberOfRowWhereToInsertData)
    output(numberOfRowWhereToInsertData) = dataToInsert
    if (output.length - (numberOfRowWhereToInsertData + 1) >= 0) arraycopy(
      originalData,
      numberOfRowWhereToInsertData + 1 - 1,
      output,
      numberOfRowWhereToInsertData + 1,
      output.length - (numberOfRowWhereToInsertData + 1)
    )
    output
  }

  def expandUniverse(galaxy: Array[Array[Char]]): Array[Array[Char]] =
    var newGalaxy = Array.empty[Array[Char]]
    var i         = 0
    galaxy.foreach: line =>
      newGalaxy = insertRow(newGalaxy, i, line)
      i += 1
      if line.forall(c => c == '.') then
        newGalaxy = insertRow(newGalaxy, i, line)
        i += 1
    newGalaxy

  var grid: Array[Array[Char]] = Array.fill(140)(Array.fill(140)(('.')))

  def initializeGrid(lineWithIndex: (String, Int)): Unit =
    for
      x <- 0.until(lineWithIndex._1.length)
    yield grid(lineWithIndex._2)(x) = lineWithIndex._1.charAt(x)

  def calculateOlderUniverse(): BigInt =
    importLines()
    val unexpandedUniverse = calculateDistance(generateCombinations(findGalaxies(grid)))
    val expandedUniverse   = calculateDistance(
      generateCombinations(findGalaxies(transposeMatrix(expandUniverse(transposeMatrix(expandUniverse(grid))))))
    )
    // the diff is how much is added for every expansion, so the total becomes: 
    BigInt(unexpandedUniverse) + (BigInt(expandedUniverse - unexpandedUniverse) * BigInt(999999))

  def importLines(): Unit =
    Using.resource(Source.fromResource("2023/day11input.txt")): source =>
      source.getLines().toSeq.zipWithIndex.map(initializeGrid)
end Day11
