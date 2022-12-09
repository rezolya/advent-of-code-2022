import scala.compat.Platform.EOL
import scala.io.Source

val day = 7

val filename = s"input/input$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

//val inputIterator: Iterator[String] = Seq(
//  "$ cd /",
//  "$ ls",
//  "dir a",
//  "14848514 b.txt",
//  "8504156 c.dat",
//  "dir d",
//  "$ cd a",
//  "$ ls",
//  "dir e",
//  "29116 f",
//  "2557 g",
//  "62596 h.lst",
//  "$ cd e",
//  "$ ls",
//  "584 i",
//  "$ cd ..",
//  "$ cd ..",
//  "$ cd d",
//  "$ ls",
//  "4060174 j",
//  "8033020 d.log",
//  "5626152 d.ext",
//  "7214296 k"
//).iterator

sealed trait Node {

  def size: Int
  def fullName: String
  def prepend: String = " " * fullName.count(_ == '/')
  def filter(f: Node => Boolean): Seq[Node]
}

case class Dir(name: String, fullName: String, nodes: Seq[Node]) extends Node {

  override def size: Int = nodes.map(_.size).sum

  override def toString: String =
    s"$prepend- $name (dir size = $size) [${fullName.mkString("/")}]$EOL${nodes.mkString(EOL)}"

  override def filter(f: Node => Boolean): Seq[Node] = {
//    println(s"directory $fullName size = $size")
    if (f(this)) this +: nodes.flatMap(_.filter(f)) else nodes.flatMap(_.filter(f))
  }
}

case class File(name: String, fullName: String, size: Int) extends Node {

  override def toString: String = s"$prepend- $name (file, size=$size)"

  override def filter(f: Node => Boolean): Seq[Node] = {
    if (f(this)) Seq(this) else Seq.empty
  }
}

val cdPattern = "\\$ cd (.*)".r
val dirPattern = "dir (.*)".r
val filePattern = "(\\d*) (.*)".r

def recurse(
  input: Iterator[String],
  currentDir: Seq[String],
  nodes: Seq[Node]
): (Iterator[String], Seq[String], Seq[Node]) = {
  if (!input.hasNext) {
    (input, currentDir, nodes)
  } else {
    val current = input.next()
    current match {
      case "$ cd .." => (input, currentDir.dropRight(1), nodes)
      case cdPattern(dirName) => recurse(input, currentDir :+ dirName, nodes)
      case dirPattern(dirName) => recurse(input, currentDir, nodes) // we can just ignore dirs
      case filePattern(size, fileName) =>
        val file = File(fileName, (currentDir :+ fileName).mkString("/"), size.toInt)
        recurse(input, currentDir, nodes :+ file)
      case "$ ls" =>
        val (it, cd, ns) = recurse(input, currentDir, Seq.empty)
        val dir = Dir(currentDir.last, currentDir.mkString("/"), ns)
        recurse(it, cd, nodes :+ dir)
    }
  }
}

val (iterator, currentDir, nodes) = recurse(inputIterator, Seq.empty, Seq.empty)

val filtered = nodes.head.filter(n => n.size <= 100000).collect { case n: Dir => n}
val ex1 = filtered.map(_.size).sum

println(filtered)
println(nodes)

val totalSize = 70000000
val totalNeed = 30000000
val totalUsedSize = nodes.head.size
val unused = totalSize - totalUsedSize
val needToFree = totalNeed - unused

val moreThanNeedToFree = nodes.head.filter(_.size >= needToFree).collect { case n: Dir => n }.map(_.size).min

bufferedSource.close()

