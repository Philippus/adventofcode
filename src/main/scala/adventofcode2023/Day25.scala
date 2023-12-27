package adventofcode2023

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day25:
  import Dijkstra._
  import Dijkstra.EdgeWeightedDigraphOps._

  case class Connection(component1: String, component2: String)

  def getComponents(connections: Set[Connection]): Set[String] =
    connections.flatMap(c => Set(c.component1, c.component2))

  def findGroupOfComponents(component: String, connections: Set[Connection]): Set[String] =
    @tailrec
    def loop(toCheckComponents: Set[String], checkedComponents: Set[String]): Set[String] =
      if toCheckComponents.isEmpty then
        checkedComponents
      else
        val toCheckComponent                 = toCheckComponents.head
        val connectedComponents: Set[String] =
          connections
            .filter(c => c.component1 == toCheckComponent || c.component2 == toCheckComponent).flatMap(c =>
              Set(c.component1, c.component2)
            )
            .filterNot(c => checkedComponents.contains(c) || c == toCheckComponent)
        loop(toCheckComponents.tail ++ connectedComponents, checkedComponents + toCheckComponent)
    loop(Set(component), Set.empty)

  def findMultipleOfGroupSizes(components: Set[String], connections: Seq[Connection], topN: Int): Int =
    val componentsWithId = components.zipWithIndex

    val edges = connections.flatMap { line =>
      Set(
        DirectedEdge(
          componentsWithId.find(_._1 == line.component1).get._2,
          componentsWithId.find(_._1 == line.component2).get._2,
          1
        ),
        DirectedEdge(
          componentsWithId.find(_._1 == line.component2).get._2,
          componentsWithId.find(_._1 == line.component1).get._2,
          1
        )
      )
    }

    val graph = EdgeWeightedDigraph()
      .addEdges(edges)

    val paths = componentsWithId.grouped(2).map { pair =>
      val sp   = ShortestPath.run(graph, pair.head._2)
      val path = sp.toOption.get.pathTo(pair.last._2)
      path.toOption.get
    }

    val visitedConnections =
      for
        path <- paths
        edge <- path
        c1    = componentsWithId.find(_._2 == edge.from).get._1
        c2    = componentsWithId.find(_._2 == edge.to).get._1
      yield connections.find(l =>
        (l.component1 == c1 && l.component2 == c2) || (l.component1 == c2 && l.component2 == c1)
      ).get

    val topNVisitedConnections =
      visitedConnections.toSeq.groupBy(identity).toSeq.sortBy(-_._2.size).take(topN).map(_._1)

    val sizeOfFirstGroup = topNVisitedConnections.combinations(3).map: combo =>
      findGroupOfComponents(combo.head.component1, connections.diff(combo).toSet).size
    .filter(_.<(components.size))
      .take(1)
      .toSeq
      .head

    sizeOfFirstGroup * (components.size - sizeOfFirstGroup)

  def handleLine(line: String): Set[Connection] = line match
    case s"$w1: $wires" =>
      val splits = wires.split(' ')
      splits.map(w => Connection(w1, w)).toSet

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource("2023/day25input.txt")):
      _.getLines().toSeq
end Day25
