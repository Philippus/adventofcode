package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09:
  def createBlocks(diskmap: String): Seq[Int] =
    diskmap.grouped(2).zipWithIndex.toSeq.flatMap:
      case s if s._1.length == 1 => Seq.fill(s._1.toInt)(s._2)
      case s                     => Seq.fill(s._1.head.asDigit)(s._2) ++ Seq.fill(s._1.last.asDigit)(-1)

  @tailrec
  def moveBlocks(blocks: Seq[Int]): Seq[Int] =
    if blocks.indexWhere(_.==(-1)) > blocks.lastIndexWhere(!_.==(-1)) then
      blocks
    else
      moveBlocks(blocks
        .updated(blocks.indexWhere(_.==(-1)), blocks(blocks.lastIndexWhere(!_.==(-1))))
        .updated(blocks.lastIndexWhere(!_.==(-1)), -1))

  // warning: this takes forever, but it works
  def moveBlocksPartTwo(blocks: Seq[Int]): Seq[Int] =
    @tailrec
    def loop(blocks: Seq[Int], files: Seq[Int]): Seq[Int] =
      if files.isEmpty then
        blocks
      else
        val fiw          = blocks.indexWhere(_.==(files.last))
        val liw          = blocks.lastIndexWhere(_.==(files.last))
        val lengthOfFile = liw - fiw + 1
        blocks.indexOfSlice(Seq.fill(lengthOfFile)(-1)) match
          case -1           =>
            loop(blocks, files.init)
          case i if i < liw =>
            loop(
              blocks.take(i) ++
                Seq.fill(lengthOfFile)(files.last) ++
                blocks.slice(i + lengthOfFile, fiw) ++
                Seq.fill(lengthOfFile)(-1) ++
                blocks.drop(liw + 1),
              files.init
            )
          case _            =>
            loop(blocks, files.init)

    val files = blocks.distinct.filterNot(_.==(-1))
    loop(blocks, files)

  def fileChecksum(blocks: Seq[Int]): Long =
    blocks.zipWithIndex.map:
      case (block, index) => if (block != -1) then index.toLong * block.toLong else 0L
    .sum

  def importLines(): String =
    Using.resource(Source.fromResource(s"2024/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().next()
end Day09
