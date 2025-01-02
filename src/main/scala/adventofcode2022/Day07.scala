package adventofcode2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

import adventofcode2022.Day07.FileOrDir.*

object Day07:
  enum FileOrDir:
    def getParent: FileOrDir =
      this match
        case Root              => Root
        case f @ File(_, _, _) => f.parent
        case d @ Dir(_, _)     => d.parent
    case Root extends FileOrDir
    case File(name: String, fileSize: Long, parent: FileOrDir) extends FileOrDir
    case Dir(name: String, parent: FileOrDir)                  extends FileOrDir
  end FileOrDir

  def handleCommands(commands: Seq[String]): Seq[FileOrDir] =
    @tailrec
    def list(commands: Seq[String], currentDir: FileOrDir, acc: Seq[FileOrDir]): Seq[FileOrDir] =
      if commands.isEmpty then
        acc
      else
        commands.head match
          case c if c.startsWith("$") => loop(commands, currentDir, acc)
          case s"dir $name"           =>
            val newDir = Dir(name, currentDir)
            list(commands.tail, currentDir, acc :+ newDir)
          case s"$fileSize $fileName" =>
            val newFile = File(fileName, fileSize.toLong, currentDir)
            list(commands.tail, currentDir, acc :+ newFile)

    @tailrec
    def loop(commands: Seq[String], currentDir: FileOrDir, acc: Seq[FileOrDir]): Seq[FileOrDir] =
      if commands.isEmpty then
        acc
      else
        commands.head match
          case "$ cd /"      => loop(commands.tail, Root, acc)
          case "$ cd .."     => loop(commands.tail, currentDir.getParent, acc)
          case "$ ls"        => list(commands.tail, currentDir, acc)
          case s"$$ cd $dir" => loop(commands.tail, Dir(dir, currentDir), acc)

    loop(commands, Root, Seq.empty[FileOrDir])

  def sumSizeOfDirectoriesOfMax100000(acc: Seq[FileOrDir]): Long =
    def sizeOfDir(dir: Dir): Long =
      acc.filter(_.getParent == dir).map {
        case f @ File(_, fileSize, _) =>
          fileSize
        case d @ Dir(_, _)            =>
          sizeOfDir(d)
      }
        .sum
    acc.collect:
      case d @ Dir(_, _) if sizeOfDir(d) <= 100000 => sizeOfDir(d)
    .sum

  def sizeOfDirectoryToDelete(acc: Seq[FileOrDir]): Long =
    def sizeOfDir(dir: FileOrDir): Long =
      acc.filter(_.getParent == dir).map:
        case f @ File(_, fileSize, _) =>
          fileSize
        case d @ Dir(_, _)            =>
          sizeOfDir(d)
      .sum

    val unused = 70000000L - sizeOfDir(Root)

    acc.collect:
      case d @ Dir(_, _) if (sizeOfDir(d) + unused) >= 30000000L => sizeOfDir(d)
    .min

  def importLines(): Seq[String] =
    Using.resource(Source.fromResource(s"2022/${this.getClass.getSimpleName.toLowerCase.replace("$", "")}input.txt")):
      source =>
        source.getLines().toSeq
end Day07
