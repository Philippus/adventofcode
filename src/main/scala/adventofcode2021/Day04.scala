package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day04:
  case class Board(board: Vector[Vector[Int]])

  def isWinner(board: Board, drawnNumbers: Seq[Int]) =
    val h =
      for
        x <- board.board.head.indices
        if drawnNumbers.contains(board.board(x)(0)) &&
          drawnNumbers.contains(board.board(x)(1)) &&
          drawnNumbers.contains(board.board(x)(2)) &&
          drawnNumbers.contains(board.board(x)(3)) &&
          drawnNumbers.contains(board.board(x)(4))
      yield true

    val v =
      for
        y <- board.board.head.indices
        if drawnNumbers.contains(board.board(0)(y)) &&
          drawnNumbers.contains(board.board(1)(y)) &&
          drawnNumbers.contains(board.board(2)(y)) &&
          drawnNumbers.contains(board.board(3)(y)) &&
          drawnNumbers.contains(board.board(4)(y))
      yield true

    h.contains(true) || v.contains(true)

  def score(board: Board, drawnNumbers: Seq[Int]): Int =
    board.board.flatten.filter(!drawnNumbers.contains(_)).sum * drawnNumbers.last

  def determineWinningScore(boards: Seq[Board], drawnNumbers: Seq[Int]): Int =
    @tailrec
    def loop(draw: Int): Int =
      val winningBoards = boards.filter(board => isWinner(board, drawnNumbers.take(draw)))
      if winningBoards.nonEmpty then
        score(winningBoards.head, drawnNumbers.take(draw))
      else
        loop(draw + 1)
    loop(1)

  def determineLastToWin(boards: Seq[Board], drawnNumbers: Seq[Int]): Int =
    @tailrec
    def loop(boards: Seq[Board], draw: Int, lastScoreOfWinningBoard: Int): Int =
      if boards.isEmpty then
        lastScoreOfWinningBoard
      else
        val winningBoards = boards.filter(board => isWinner(board, drawnNumbers.take(draw)))
        if winningBoards.nonEmpty then
          val scoreOfWinningBoard = score(winningBoards.head, drawnNumbers.take(draw))
          loop(boards.diff(winningBoards), draw + 1, scoreOfWinningBoard)
        else
          loop(boards, draw + 1, lastScoreOfWinningBoard)

    loop(boards, 1, 0)

  def importLines(): (Seq[Int], Seq[Board]) =
    Using.resource(Source.fromResource("2021/day04input.txt")): source =>
      val lines        = source.getLines().toSeq.filter(_.nonEmpty)
      val drawnNumbers = lines.head.split(',').map(_.toInt).toSeq
      val boards       = lines.tail.grouped(5).map(s =>
        Vector(s.map(s => s.trim.split("\\s+").toVector.map(_.toInt))*)
      ).toSeq.map(Board.apply)
      (drawnNumbers, boards)
end Day04
