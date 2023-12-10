package adventofcode2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day19:
  def replace(molecule: String, replacement: (String, String)): Seq[String] =
    val pattern = replacement._1.r
    val indices = pattern.findAllMatchIn(molecule).map(_.start)
    (indices.map: index =>
      molecule.substring(0, index) ++ replacement._2 ++ molecule.substring(index + replacement._1.length)).toSeq

  // this doesn't work for the input file, it will take forever,
  // found the answer with a bit of trial and error, looking at the input file, Ar , Rn and Y have a special meaning.
  def stepsToMedicine(medicineMolecule: String, replacements: Seq[(String, String)]): Int =
    @tailrec
    def loop(candidates: Seq[String], acc: Int): Int =
      if candidates.contains(medicineMolecule) then
        acc
      else
        val r = candidates.flatMap(candidate => replacements.flatMap(x => replace(candidate, x)).distinct).distinct
        loop(r, acc + 1)
    loop(Seq("e"), 0)

  def handleLine(line: String): (String, String) =
    line match
      case s"$source => $target" =>
        source -> target

  def countDistinctMoleculesForFile(): Int =
    Using.resource(Source.fromResource("2015/day19input.txt")): source =>
      val lines = source.getLines().toSeq

      val replacements     = lines.dropRight(2).map(handleLine)
      val medicineMolecule = lines.last
      val result           = replacements.flatMap(x => replace(medicineMolecule, x)).distinct
      result.length
end Day19
