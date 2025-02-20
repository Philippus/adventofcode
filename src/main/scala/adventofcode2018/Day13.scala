package adventofcode2018

//import scala.collection.mutable.Map
import scala.io.Source
import scala.util.Using

object Day13:
  case class Pos(x: Int, y: Int):
    def north: Pos = Pos(x, y - 1)
    def east: Pos  = Pos(x + 1, y)
    def south: Pos = Pos(x, y + 1)
    def west: Pos  = Pos(x - 1, y)
  end Pos

  def moveCarts(
      map: collection.immutable.Map[Pos, Char],
      carts: Vector[((Pos, Char), Int)],
      removeCarts: Boolean = false
  ): Pos =
    def loop(carts: Vector[((Pos, Char), Int)]): Pos =
      if removeCarts && carts.length == 1 then
        carts.head._1._1
      else
        val newCarts              = carts.sortBy(c => (c._1._1.y, c._1._1.x)).to(collection.mutable.ListBuffer)
        var crashPos: Option[Pos] = None
        carts.foreach: cart =>
          if newCarts.contains(cart) then
            cart match
              case ((p, '^'), _) if newCarts.exists(_._1._1 == p.north) =>
                if !removeCarts then
                  crashPos = crashPos.orElse(Some(p.north)) // crash
                else
                  newCarts.remove(newCarts.indexWhere(cart => cart._1._1 == p))
                  newCarts.remove(newCarts.indexWhere(cart => cart._1._1 == p.north))
              case ((p, '^'), turn)                                     => map(p.north) match
                  case '|'  => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '^')), ((p.north, '^'), turn))
                  case '/'  => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '^')), ((p.north, '>'), turn))
                  case '\\' => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '^')), ((p.north, '<'), turn))
                  case '+'  => turn % 3 match
                      case 0 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '^')), ((p.north, '<'), turn + 1))
                      case 1 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '^')), ((p.north, '^'), turn + 1))
                      case 2 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '^')), ((p.north, '>'), turn + 1))
              // intersection
              case ((p, '>'), _) if newCarts.exists(_._1._1 == p.east)  =>
                if !removeCarts then
                  crashPos = crashPos.orElse(Some(p.east)) // crash
                else
                  newCarts.remove(newCarts.indexWhere(cart => cart._1._1 == p))
                  newCarts.remove(newCarts.indexWhere(cart => cart._1._1 == p.east))
              case ((p, '>'), turn)                                     => map(p.east) match
                  case '-'  => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '>')), ((p.east, '>'), turn))
                  case '/'  => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '>')), ((p.east, '^'), turn))
                  case '\\' => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '>')), ((p.east, 'v'), turn))
                  case '+'  => turn % 3 match
                      case 0 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '>')), ((p.east, '^'), turn + 1))
                      case 1 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '>')), ((p.east, '>'), turn + 1))
                      case 2 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '>')), ((p.east, 'v'), turn + 1))
              case ((p, 'v'), _) if newCarts.exists(_._1._1 == p.south) =>
                if !removeCarts then
                  crashPos = crashPos.orElse(Some(p.south)) // crash
                else
                  newCarts.remove(newCarts.indexWhere(cart => cart._1._1 == p))
                  newCarts.remove(newCarts.indexWhere(cart => cart._1._1 == p.south))
              case ((p, 'v'), turn)                                     => map(p.south) match
                  case '|'  => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, 'v')), ((p.south, 'v'), turn))
                  case '/'  => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, 'v')), ((p.south, '<'), turn))
                  case '\\' => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, 'v')), ((p.south, '>'), turn))
                  case '+'  => turn % 3 match
                      case 0 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, 'v')), ((p.south, '>'), turn + 1))
                      case 1 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, 'v')), ((p.south, 'v'), turn + 1))
                      case 2 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, 'v')), ((p.south, '<'), turn + 1))
              case ((p, '<'), _) if newCarts.exists(_._1._1 == p.west)  =>
                if !removeCarts then
                  crashPos = crashPos.orElse(Some(p.west)) // crash
                else
                  newCarts.remove(newCarts.indexWhere(cart => cart._1._1 == p))
                  newCarts.remove(newCarts.indexWhere(cart => cart._1._1 == p.west))
              case ((p, '<'), turn)                                     => map(p.west) match
                  case '-'  => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '<')), ((p.west, '<'), turn))
                  case '/'  => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '<')), ((p.west, 'v'), turn))
                  case '\\' => newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '<')), ((p.west, '^'), turn))
                  case '+'  => turn % 3 match
                      case 0 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '<')), ((p.west, 'v'), turn + 1))
                      case 1 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '<')), ((p.west, '<'), turn + 1))
                      case 2 =>
                        newCarts.update(newCarts.indexWhere(cart => cart._1 == (p, '<')), ((p.west, '^'), turn + 1))
        crashPos.getOrElse(loop(newCarts.toVector))
    loop(carts)

  def readCarts(line: String, y: Int): Vector[((Pos, Char), Int)] =
    line.zipWithIndex.toVector.collect:
      case (c, x) if Seq('^', '>', 'v', '<').contains(c) => ((Pos(x, y), c), 0)

  def parse(line: String, y: Int): Map[Pos, Char] =
    line.zipWithIndex.collect:
      case ('^', x)           =>
        Pos(x, y) -> '|'
      case ('>', x)           =>
        Pos(x, y) -> '-'
      case ('v', x)           =>
        Pos(x, y) -> '|'
      case ('<', x)           =>
        Pos(x, y) -> '-'
      case (c, x) if c != ' ' =>
        Pos(x, y) -> c
    .toMap

  def handleLines(lines: Vector[String]): (Map[Pos, Char], Vector[((Pos, Char), Int)]) =
    val map   = lines.zipWithIndex.flatMap(l => parse(l._1, l._2)).toMap
    val carts = lines.zipWithIndex.flatMap(l => readCarts(l._1, l._2))
    (map, carts)

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day13
