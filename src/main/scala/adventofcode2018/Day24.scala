package adventofcode2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day24:
  enum AttackType:
    case Radiation, Fire, Slashing, Cold, Bludgeoning
  end AttackType

  case object AttackType:
    def fromString(str: String): AttackType =
      str.trim match
        case "radiation"   => Radiation
        case "fire"        => Fire
        case "slashing"    => Slashing
        case "cold"        => Cold
        case "bludgeoning" => Bludgeoning
  end AttackType

  case class Group(
      id: Int,
      army: Int,
      units: Int,
      hitPoints: Int,
      attackDamage: Int,
      attackType: AttackType,
      initiative: Int,
      weaknesses: Vector[AttackType],
      immunities: Vector[AttackType]
  ):
    val effectivePower: Int = units * attackDamage

    def damageTo(target: Group): Int =
      if target.weaknesses.contains(attackType) then effectivePower * 2
      else if target.immunities.contains(attackType) then 0
      else effectivePower
    def killing(target: Group): Int  =
      if (damageTo(target) / target.hitPoints) > target.units then
        target.units
      else
        damageTo(target) / target.hitPoints
  end Group

  case class Army(name: String, groups: Vector[Group])

  def battle(army1: Army, army2: Army): Int =
    @tailrec
    def fight(army1: Army, army2: Army): Int =
      if !army1.groups.exists(_.units > 0) || !army2.groups.exists(_.units > 0) then
        army1.groups.map(_.units).sum + army2.groups.map(_.units).sum
      else
        println(s"${army1.name}:")
        army1.groups.foreach: g =>
          println(s"Group ${g.id} contains ${g.units} units")
        println(s"${army2.name}:")
        army2.groups.foreach: g =>
          println(s"Group ${g.id} contains ${g.units} units")

        // target selection
        val groups1 =
          army1.groups.filter(_.units > 0).sortBy(g => (-g.effectivePower, -g.initiative)).to(mutable.ListBuffer)
        val groups2 =
          army2.groups.filter(_.units > 0).sortBy(g => (-g.effectivePower, -g.initiative)).to(mutable.ListBuffer)

        val candidatesForArmy1 = groups2
        val selectedByArmy1    = collection.mutable.ListBuffer.empty[(Group, Group)]
        groups1.foreach: group =>
          val targetOpt = candidatesForArmy1.filterNot(g => selectedByArmy1.exists(_._2 == g)).filter(g =>
            group.damageTo(g) > 0
          ).sortBy(g => (-group.damageTo(g), -g.effectivePower, -g.initiative)).headOption
          if targetOpt.nonEmpty then
            selectedByArmy1.addOne((group, targetOpt.get))
            ()
          else
            (
          )
        selectedByArmy1.foreach: (s, t) =>
          println(s"${army1.name} group ${s.id} would deal defending group ${t.id} ${s.damageTo(t)} damage")

        val candidatesForArmy2 = groups1
        val selectedByArmy2    = collection.mutable.ListBuffer.empty[(Group, Group)]
        groups2.foreach: group =>
          val targetOpt = candidatesForArmy2.filterNot(g => selectedByArmy2.exists(_._2 == g)).filter(g =>
            group.damageTo(g) > 0
          ).sortBy(g => (-group.damageTo(g), -g.effectivePower, -g.initiative)).headOption
          if targetOpt.nonEmpty then
            selectedByArmy2.addOne((group, targetOpt.get))
            ()
          else
            (
          )
        selectedByArmy2.foreach: (s, t) =>
          println(s"${army2.name} group ${s.id} would deal defending group ${t.id} ${s.damageTo(t)} damage")

        // attacking phase
        val newGroups1 = groups1
        val newGroups2 = groups2
        (selectedByArmy1 ++ selectedByArmy2).sortBy(g => -g._1.initiative).foreach: g =>
          if g._1.army == 1 then
            val a = newGroups1.find(_.id == g._1.id).get
            val d = newGroups2.find(_.id == g._2.id).get
            println(s"Immune system group ${a.id} attacks defending group ${d.id}, killing ${a.killing(d)} units")
            newGroups2.update(
              newGroups2.indexWhere(_.id == g._2.id),
              newGroups2.find(_.id == g._2.id).get.copy(units =
                newGroups2.find(_.id == g._2.id).get.units - a.killing(d)
              )
            )
          else
            val a = newGroups2.find(_.id == g._1.id).get
            val d = newGroups1.find(_.id == g._2.id).get
            println(s"Infection group ${a.id} attacks defending group ${d.id}, killing ${a.killing(d)} units")
            newGroups1.update(
              newGroups1.indexWhere(_.id == g._2.id),
              newGroups1.find(_.id == g._2.id).get.copy(units =
                newGroups1.find(_.id == g._2.id).get.units - a.killing(d)
              )
            )
        fight(Army(army1.name, newGroups1.toVector), Army(army2.name, newGroups2.toVector))
    fight(army1, army2)

  def battleWithBoost(army1: Army, army2: Army): Int =
    @tailrec
    def fight(army1: Army, army2: Army): Option[Int] =
      if !army1.groups.exists(_.units > 0) || !army2.groups.exists(_.units > 0) then
        if army1.groups.exists(_.units > 0) then
          Some(army1.groups.map(_.units).sum)
        else
          None
      else
        println(s"${army1.name}:")
        army1.groups.foreach: g =>
          println(s"Group ${g.id} contains ${g.units} units")
        println(s"${army2.name}:")
        army2.groups.foreach: g =>
          println(s"Group ${g.id} contains ${g.units} units")

        // target selection
        val groups1 =
          army1.groups.filter(_.units > 0).sortBy(g => (-g.effectivePower, -g.initiative)).to(mutable.ListBuffer)
        val groups2 =
          army2.groups.filter(_.units > 0).sortBy(g => (-g.effectivePower, -g.initiative)).to(mutable.ListBuffer)

        val candidatesForArmy1 = groups2
        val selectedByArmy1    = collection.mutable.ListBuffer.empty[(Group, Group)]
        groups1.foreach: group =>
          val targetOpt = candidatesForArmy1.filterNot(g => selectedByArmy1.exists(_._2 == g)).filter(g =>
            group.damageTo(g) > 0
          ).sortBy(g => (-group.damageTo(g), -g.effectivePower, -g.initiative)).headOption
          if targetOpt.nonEmpty then
            selectedByArmy1.addOne((group, targetOpt.get))
        selectedByArmy1.foreach: (s, t) =>
          println(s"${army1.name} group ${s.id} would deal defending group ${t.id} ${s.damageTo(t)} damage")

        val candidatesForArmy2 = groups1
        val selectedByArmy2    = collection.mutable.ListBuffer.empty[(Group, Group)]
        groups2.foreach: group =>
          val targetOpt = candidatesForArmy2.filterNot(g => selectedByArmy2.exists(_._2 == g)).filter(g =>
            group.damageTo(g) > 0
          ).sortBy(g => (-group.damageTo(g), -g.effectivePower, -g.initiative)).headOption
          if targetOpt.nonEmpty then
            selectedByArmy2.addOne((group, targetOpt.get))
        selectedByArmy2.foreach: (s, t) =>
          println(s"${army2.name} group ${s.id} would deal defending group ${t.id} ${s.damageTo(t)} damage")

        // attacking phase
        val newGroups1 = groups1
        val newGroups2 = groups2
        (selectedByArmy1 ++ selectedByArmy2).sortBy(g => -g._1.initiative).foreach: g =>
          if g._1.army == 1 then
            val a = newGroups1.find(_.id == g._1.id).get
            val d = newGroups2.find(_.id == g._2.id).get
            println(s"Immune system group ${a.id} attacks defending group ${d.id}, killing ${a.killing(d)} units")
            newGroups2.update(
              newGroups2.indexWhere(_.id == g._2.id),
              newGroups2.find(_.id == g._2.id).get.copy(units =
                newGroups2.find(_.id == g._2.id).get.units - a.killing(d)
              )
            )
          else
            val a = newGroups2.find(_.id == g._1.id).get
            val d = newGroups1.find(_.id == g._2.id).get
            println(s"Infection group ${a.id} attacks defending group ${d.id}, killing ${a.killing(d)} units")
            newGroups1.update(
              newGroups1.indexWhere(_.id == g._2.id),
              newGroups1.find(_.id == g._2.id).get.copy(units =
                newGroups1.find(_.id == g._2.id).get.units - a.killing(d)
              )
            )
        if army1 == Army(army1.name, newGroups1.toVector) && army2 == Army(army2.name, newGroups2.toVector) then
          None // stand-off
        else
          fight(Army(army1.name, newGroups1.toVector), Army(army2.name, newGroups2.toVector))

    var boost              = 1
    var found: Option[Int] = None
    while found.isEmpty do
      val boostedGroups = army1.groups.map(g => g.copy(attackDamage = g.attackDamage + boost))
      found = fight(Army(army1.name, boostedGroups), army2)
      boost += 1
    found.get

  def handleLines(lines: Vector[String]): (Army, Army) =
    def parseWeaknessesAndImmunities(str: String): (Vector[AttackType], Vector[AttackType]) =
      val immunities =
        str.indexOf("immune to ") match
          case -1                        =>
            Vector.empty[AttackType]
          case n if str.indexOf(";") > n =>
            val is = str.substring("immune to".length, str.indexOf(";")).split(",")
            is.map(AttackType.fromString).toVector
          case n                         =>
            val is = str.substring(n + "immune to".length).split(",")
            is.map(AttackType.fromString).toVector
      val weaknesses =
        str.indexOf("weak to ") match
          case -1                        =>
            Vector.empty[AttackType]
          case n if str.indexOf(";") > n =>
            val ws = str.substring("weak to".length, str.indexOf(";")).split(",")
            ws.map(AttackType.fromString).toVector
          case n                         =>
            val ws = str.substring(n + "weak to".length).split(",")
            ws.map(AttackType.fromString).toVector
      (immunities, weaknesses)

    def parseGroup(line: String, idx: Int, army: Int): Group =
      line match
        case s"$units units each with $hitPoints hit points with an attack that does $attackDamage $attackType damage at initiative $initiative"       =>
          Group(
            idx,
            army,
            units.toInt,
            hitPoints.toInt,
            attackDamage.toInt,
            AttackType.fromString(attackType),
            initiative.toInt,
            Vector.empty[AttackType],
            Vector.empty[AttackType]
          )
        case s"$units units each with $hitPoints hit points ($wi) with an attack that does $attackDamage $attackType damage at initiative $initiative" =>
          val (immunities, weaknesses) = parseWeaknessesAndImmunities(wi)
          Group(
            idx,
            army,
            units.toInt,
            hitPoints.toInt,
            attackDamage.toInt,
            AttackType.fromString(attackType),
            initiative.toInt,
            weaknesses,
            immunities
          )

    def parseArmy(lines: Vector[String], army: Int): Army =
      Army(lines.head.init, lines.zipWithIndex.tail.map(l => parseGroup(l._1, l._2, army)))

    val armies = lines.splitAt(lines.indexWhere(_.isEmpty))
    (parseArmy(armies._1.filter(_.nonEmpty), 1), parseArmy(armies._2.filter(_.nonEmpty), 2))

  def importLines(): Vector[String] =
    Using.resource(Source.fromResource(s"2018/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toVector
end Day24
