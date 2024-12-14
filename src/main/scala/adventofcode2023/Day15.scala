package adventofcode2023

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

object Day15:
  case class Lens(label: String, focalLength: Long)

  case class Box(lenses: Seq[Lens])

  def calculateHash(hash: String): Long =
    var currentValue = 0L
    hash.foreach: char =>
      currentValue += char.toInt.toLong
      currentValue = currentValue * 17L
      currentValue = currentValue % 256L
    currentValue

  def sumOfHashes(steps: String): Long =
    steps.split(',').toSeq.map(calculateHash).sum

  def fillBoxes(steps: String): Seq[Box] =
    val boxes = new ArrayBuffer[Box](256)
    for
      i <- 0 until 256
    do
      boxes += Box(Seq.empty[Lens])

    steps.split(',').toSeq.foreach:
      case s"$label=$focalLength" =>
        val boxId = calculateHash(label).toInt
        val box   = boxes(boxId)
        if box.lenses.exists(lens => lens.label == label) then
          boxes.update(
            boxId,
            box.copy(lenses =
              box.lenses.updated(box.lenses.indexWhere(lens => lens.label == label), Lens(label, focalLength.toLong))
            )
          )
        else
          boxes.update(
            boxId,
            box.copy(lenses =
              box.lenses :+ Lens(label, focalLength.toLong)
            )
          )
      case s"$label-"             =>
        val boxId = calculateHash(label).toInt
        val box   = boxes(boxId)
        boxes.update(boxId, box.copy(lenses = box.lenses.filterNot(lens => lens.label == label)))
    boxes.toSeq

  def calculateFocusingPower(boxes: Seq[Box]): Long =
    (for
      (box, i) <- boxes.zipWithIndex
      if box.lenses.nonEmpty
    yield (i + 1) * (box.lenses.zipWithIndex.map:
      case (lens, j) => (j + 1) * lens.focalLength
    ).sum).sum

  def importLines(): String =
    Using.resource(Source.fromResource(s"2023/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq.head
end Day15
