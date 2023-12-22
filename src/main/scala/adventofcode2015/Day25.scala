package adventofcode2015

object Day25:
  val startCode: BigInt = 20151125L

  def determineNextCode(previousCode: BigInt): BigInt =
    (previousCode * 252533L) % 33554393

  def findCodeByRowAndColumn(targetRow: Int, targetColumn: Int): BigInt =
    def loop(row: Int, column: Int, currentCode: BigInt): BigInt =
      if row == targetRow && column == targetColumn then
        currentCode
      else
        val (nextRow, nextColumn) = (row, column) match
          case (1, column)   => (column + 1, 1)
          case (row, column) => (row - 1, column + 1)
        loop(nextRow, nextColumn, determineNextCode(currentCode))

    loop(1, 1, startCode)
end Day25
