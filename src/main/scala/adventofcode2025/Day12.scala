package adventofcode2025

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12:
  case class Present(number: Int, shape: List[String]):
    def rotate: Present =
      this.copy(shape = shape.transpose.map(_.reverse.mkString))

    def flip: Present =
      this.copy(shape = shape.map(_.reverse.mkString))

    def occupiedSpaces: Int =
      shape.flatten.count(_.==('#'))

    def occupies: List[Pos] =
      (for
        y <- shape.indices
        x <- 0.until(shape(y).length)
        if shape(y)(x) == '#'
      yield Pos(x, y))
        .toList

  case class Region(width: Int, length: Int, quantities: List[Int]):
    val spaces: Int = width * length

  case class Pos(x: Int, y: Int)

  def placePresentOnRegion(present: Present, region: Region): List[List[Pos]] =
    (for
      y <- 0.to(region.length - 3)
      x <- 0.to(region.width - 3)
    yield present.occupies.map: pos =>
      Pos(pos.x + x, pos.y + y))
      .toList

  def allWays(present: Present, region: Region): List[List[Pos]] =
    val basic                = placePresentOnRegion(present, region)
    val rotatedOnce          = placePresentOnRegion(present.rotate, region)
    val rotatedTwice         = placePresentOnRegion(present.rotate.rotate, region)
    val rotatedThrice        = placePresentOnRegion(present.rotate.rotate.rotate, region)
    val flipped              = placePresentOnRegion(present.flip, region)
    val flippedRotatedOnce   = placePresentOnRegion(present.flip.rotate, region)
    val flippedRotatedTwice  = placePresentOnRegion(present.flip.rotate.rotate, region)
    val flippedRotatedThrice = placePresentOnRegion(present.flip.rotate.rotate.rotate, region)
    (basic ++ rotatedOnce ++ rotatedTwice ++ rotatedThrice ++ flipped ++ flippedRotatedOnce ++ flippedRotatedTwice ++ flippedRotatedThrice).distinct

  def waysInQueue(wayss: List[List[List[Pos]]]): Boolean =
    def reduceWays(ways1: List[List[Pos]], ways2: List[List[Pos]]): List[List[Pos]] =
      for
        way1 <- ways1
        way2 <- ways2
        if way1.intersect(way2).isEmpty
      yield way1 ++ way2

    @tailrec
    def loop(queue: List[List[List[Pos]]]): Boolean = {
      queue match {
        case Nil              =>
          true
        case _ :: Nil         =>
          true
        case w1 :: w2 :: rest =>
          val reduced = reduceWays(w1, w2)
          if reduced.isEmpty then
            false
          else
            loop(reduced :: rest)
      }
    }

    loop(wayss)

  def place(presents: List[Present], region: Region): Boolean =
    val presentsToFit = region.quantities.zipWithIndex.flatMap: (q, i) =>
      List.fill(q)(presents(i))
    val wayss         =
      (for
        present <- presentsToFit
      yield allWays(present, region))

    waysInQueue(wayss)

  def calculateRegionsThatFitPresents(presents: List[Present], regions: List[Region]): Int =
    // doesn't work fast enough for the input, barely works fast enough for the sample
    regions.map: region =>
      place(presents, region)
    .count(x => x)

  def calculateUpperLimitOfRegionsThatFitPresents(presents: List[Present], regions: List[Region]): Int =
    // upper limit is also the answer for the input, but not for the sample
    def mightFit(presents: List[Present], region: Region): Boolean =
      val presentsToFit = region.quantities.zipWithIndex.flatMap: (q, i) =>
        List.fill(q)(presents(i))
      presentsToFit.map(_.occupiedSpaces).sum <= region.spaces

    regions.map: region =>
      mightFit(presents, region)
    .count(x => x)

  def parse(input: String): (List[Present], List[Region]) =
    val split    = input.split("\n\n").toList
    val presents = split.init.map: s =>
      val sns    = s.split("\n").toList
      val number = sns.head.match {
        case s"$n:" => n.toInt
      }
      val shape  = sns.tail
      Present(number, shape)
    val regions  = split.last.split("\n").toList.map:
      case s"${w}x$l: $qs" =>
        Region(w.toInt, l.toInt, qs.split(' ').toList.map(_.toInt))
    (presents, regions)

  def importLines(): String =
    Using.resource(Source.fromResource("2025/day12input.txt")): source =>
      source.mkString
end Day12
