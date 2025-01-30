package adventofcode2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day21:
  enum Operation:
    case `*`, `/`, `+`, `-`
  end Operation

  object Operation:
    def fromString(s: String): Operation =
      s match
        case "*" => `*`
        case "/" => `/`
        case "+" => `+`
        case "-" => `-`
  end Operation

  sealed trait Monkey(val id: String)
  case class OpMonkey(override val id: String, lhs: Monkey, op: Operation, rhs: Monkey)   extends Monkey(id)
  case class NumberMonkey(override val id: String, value: Long)                           extends Monkey(id)
  case class UnknownMonkey(override val id: String)                                       extends Monkey(id)
  case class RootMonkey(override val id: String, lhs: Monkey, op: Operation, rhs: Monkey) extends Monkey(id)

  object Monkey:
    def showEq(monkey: Monkey): String =
      monkey match
        case OpMonkey(_, lhs, op, rhs)   => s"${showEq(lhs)}$op${showEq(rhs)}"
        case NumberMonkey(_, value)      => value.toString
        case UnknownMonkey(id)           => id
        case RootMonkey(_, lhs, op, rhs) =>
          s"${showEq(lhs)}=${showEq(rhs)}"

    def evalMonkey(monkey: Monkey): Option[Long]     =
      monkey match
        case OpMonkey(_, lhs: NumberMonkey, Operation.`*`, rhs: NumberMonkey)   =>
          Some(lhs.value * rhs.value)
        case OpMonkey(_, lhs: NumberMonkey, Operation.`/`, rhs: NumberMonkey)   =>
          Some(lhs.value / rhs.value)
        case OpMonkey(_, lhs: NumberMonkey, Operation.`+`, rhs: NumberMonkey)   =>
          Some(lhs.value + rhs.value)
        case OpMonkey(_, lhs: NumberMonkey, Operation.`-`, rhs: NumberMonkey)   =>
          Some(lhs.value - rhs.value)
        case RootMonkey(_, lhs: NumberMonkey, Operation.`*`, rhs: NumberMonkey) =>
          Some(lhs.value * rhs.value)
        case RootMonkey(_, lhs: NumberMonkey, Operation.`/`, rhs: NumberMonkey) =>
          Some(lhs.value / rhs.value)
        case RootMonkey(_, lhs: NumberMonkey, Operation.`+`, rhs: NumberMonkey) =>
          Some(lhs.value + rhs.value)
        case RootMonkey(_, lhs: NumberMonkey, Operation.`-`, rhs: NumberMonkey) =>
          Some(lhs.value - rhs.value)
        case NumberMonkey(_, value)                                             =>
          Some(value)
        case _                                                                  =>
          None
    def evalMonkey2(monkey: Monkey): Option[Boolean] =
      monkey match
        case RootMonkey(_, lhs: NumberMonkey, _, rhs: NumberMonkey) =>
          Some(lhs.value == rhs.value)
        case _                                                      =>
          None
  end Monkey

  def findRoot(monkeys: Vector[Monkey]): Long =
    def loop(monkeys: Vector[Monkey], loops: Int): Long =
      monkeys.find(_.id == "root").flatMap(Monkey.evalMonkey).getOrElse:
        val newMonkeys = monkeys.map:
          case o @ OpMonkey(id, m1: UnknownMonkey, _, _)                =>
            if monkeys.filter(_.isInstanceOf[NumberMonkey]).exists(_.id == m1.id) then
              o.copy(lhs = monkeys.find(_.id == m1.id).get)
            else
              o
          case o @ OpMonkey(id, _, _, m2: UnknownMonkey)                =>
            if monkeys.filter(_.isInstanceOf[NumberMonkey]).exists(_.id == m2.id) then
              o.copy(rhs = monkeys.find(_.id == m2.id).get)
            else
              o
          case o @ OpMonkey(id, _: NumberMonkey, _, m2: NumberMonkey)   =>
            NumberMonkey(id, Monkey.evalMonkey(o).get)
          case o @ RootMonkey(id, m1: UnknownMonkey, _, _)              =>
            if monkeys.filter(_.isInstanceOf[NumberMonkey]).exists(_.id == m1.id) then
              o.copy(lhs = monkeys.find(_.id == m1.id).get)
            else
              o
          case o @ RootMonkey(id, _, _, m2: UnknownMonkey)              =>
            if monkeys.filter(_.isInstanceOf[NumberMonkey]).exists(_.id == m2.id) then
              o.copy(rhs = monkeys.find(_.id == m2.id).get)
            else
              o
          case o @ RootMonkey(id, _: NumberMonkey, _, m2: NumberMonkey) =>
            NumberMonkey(id, Monkey.evalMonkey(o).get)
          case m                                                        =>
            m

        loop(newMonkeys, loops + 1)

    loop(monkeys, 0)

  def findHumn(monkeys: Vector[Monkey]): Long =
    @tailrec
    def loop2(monkeys: Vector[Monkey], monkey: NumberMonkey): Long =
      val nextMonkey =
        monkeys.find(_.id == monkey.id).get match
          case o @ OpMonkey(id, m1: UnknownMonkey, op: Operation, m2: NumberMonkey) =>
            op match
              case Operation.`*` =>
                NumberMonkey(m1.id, monkey.value / m2.value)
              case Operation.`/` =>
                NumberMonkey(m1.id, monkey.value * m2.value)
              case Operation.`+` =>
                NumberMonkey(m1.id, monkey.value - m2.value)
              case Operation.`-` =>
                NumberMonkey(m1.id, monkey.value + m2.value)
          case o @ OpMonkey(id, m1: NumberMonkey, op, m2: UnknownMonkey)            =>
            op match
              case Operation.`*` =>
                NumberMonkey(m2.id, monkey.value / m1.value)
              case Operation.`/` =>
                NumberMonkey(m2.id, monkey.value * m1.value)
              case Operation.`+` =>
                NumberMonkey(m2.id, monkey.value - m1.value)
              case Operation.`-` =>
                NumberMonkey(m2.id, m1.value - monkey.value) // 25 -x = 30 => 25 - 30 - x = 0 => 25 - 30 = x
      if nextMonkey.id == "humn" then
        nextMonkey.value
      else
        loop2(monkeys, nextMonkey)

    def loop(monkeys: Vector[Monkey]): Long =
      monkeys.find(_.id == "humn").flatMap(Monkey.evalMonkey).getOrElse:
        val newMonkeys = monkeys.map:
          case o @ OpMonkey(id, m1: UnknownMonkey, _, _)
              if monkeys.filter(_.isInstanceOf[NumberMonkey]).exists(_.id == m1.id) =>
            o.copy(lhs = monkeys.filter(_.isInstanceOf[NumberMonkey]).find(_.id == m1.id).get)
          case o @ OpMonkey(id, _, _, m2: UnknownMonkey)
              if monkeys.filter(_.isInstanceOf[NumberMonkey]).exists(_.id == m2.id) =>
            o.copy(rhs = monkeys.filter(_.isInstanceOf[NumberMonkey]).find(_.id == m2.id).get)
          case o @ OpMonkey(id, _: NumberMonkey, _, m2: NumberMonkey) =>
            NumberMonkey(id, Monkey.evalMonkey(o).get)
          case o @ RootMonkey(id, m1: UnknownMonkey, _, _)
              if monkeys.filter(_.isInstanceOf[NumberMonkey]).exists(_.id == m1.id) =>
            o.copy(lhs = monkeys.filter(_.isInstanceOf[NumberMonkey]).find(_.id == m1.id).get)
          case o @ RootMonkey(id, _, _, m2: UnknownMonkey)
              if monkeys.filter(_.isInstanceOf[NumberMonkey]).exists(_.id == m2.id) =>
            o.copy(rhs = monkeys.filter(_.isInstanceOf[NumberMonkey]).find(_.id == m2.id).get)
          case m                                                      =>
            m
        if newMonkeys == monkeys then
          val rootValueMonkey = monkeys.find(_.id == "root").get match
            case o @ RootMonkey(id, m1: UnknownMonkey, _, m2: NumberMonkey) =>
              NumberMonkey(m1.id, m2.value)
            case o @ RootMonkey(id, m1: NumberMonkey, _, m2: UnknownMonkey) =>
              NumberMonkey(m2.id, m1.value)
          loop2(newMonkeys, rootValueMonkey)
        else
          loop(newMonkeys)
    loop(monkeys.filterNot(_.id == "humn") :+ UnknownMonkey("humn"))

  def handleLines(importLines: Vector[String]): Vector[Monkey] =
    importLines.map:
      case s"root: $m1 $op $m2" =>
        RootMonkey("root", UnknownMonkey(m1), Operation.fromString(op), UnknownMonkey(m2))
      case s"$id: $m1 $op $m2"  =>
        OpMonkey(id, UnknownMonkey(m1), Operation.fromString(op), UnknownMonkey(m2))
      case s"$id: $l"           =>
        NumberMonkey(id, l.toLong)

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day21
