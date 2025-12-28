package adventofcode2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12:
  case class Edge(from: String, to: String)

  def pathsThatVisitSmallCavesAtMostOnce(edges: List[Edge]): List[List[String]] =
    @tailrec
    def loop(queue: List[List[String]], acc: List[List[String]]): List[List[String]] =
      if queue.isEmpty then
        acc.distinct
      else if queue.head.last == "end" then
        loop(queue.tail, acc :+ queue.head)
      else
        val head       = queue.head
        val candidates = edges
          .filter(_.from == head.last)
          .filterNot(e => e.to.forall(_.isLower) && head.contains(e.to))
        loop(queue.tail ++ candidates.map(candidate => head :+ candidate.to), acc)

    loop(List(List("start")), List.empty[List[String]])

  def pathsThatVisitOneSmallCaveAtMostTwice(edges: List[Edge]): List[List[String]] =
    @tailrec
    def containsDuplicates(path: List[String]): Boolean =
      if path.length < 2 then false
      else
        path.tail.contains(path.head) || containsDuplicates(path.tail)

    @tailrec
    def loop(queue: List[List[String]], acc: List[List[String]]): List[List[String]] =
      if queue.isEmpty then
        acc.distinct
      else if queue.head.last == "end" then
        loop(queue.tail, acc :+ queue.head)
      else
        val head       = queue.head
        val candidates = if containsDuplicates(head.filter(_.forall(_.isLower))) then
          edges
            .filter(_.from == head.last)
            .filterNot(_.to == "start")
            .filterNot(e => e.to.forall(_.isLower) && head.contains(e.to))
        else
          edges
            .filter(_.from == head.last)
            .filterNot(_.to == "start")
        loop(queue.tail ++ candidates.map(candidate => head :+ candidate.to), acc)

    loop(List(List("start")), List.empty[List[String]])

  def calculatePathsThatVisitSmallCavesAtMostOnce(edges: List[Edge]): Int =
    pathsThatVisitSmallCavesAtMostOnce(edges).length

  def calculatePathsThatVisitOneSmallCaveAtMostTwice(edges: List[Edge]): Int =
    pathsThatVisitOneSmallCaveAtMostTwice(edges).length

  def parse(input: String): List[Edge] =
    input.split('\n').toList.flatMap:
      case s"$from-$to" =>
        List(Edge(from, to), Edge(to, from))

  def importLines(): String =
    Using.resource(Source.fromResource("2021/day12input.txt")): source =>
      source.mkString
end Day12
