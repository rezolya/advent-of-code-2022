import scala.annotation.tailrec
import scala.io.Source

val day = 13

val filename = s"input/input$day.txt"
//val filename = s"input/testInput$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator = bufferedSource.getLines()

case class Pair(left: String, right: String) {
  override def toString: String = s"Pair(\n     $left\n     $right)"
}

implicit class StringOps(self: String) {

  private def takeInt(chars: Seq[Char]): (Int, Seq[Char]) = {
//    println(s"takeInt from ${array.mkString("")}")
    val nonNumbers = "[],"
    val firstNonNumber = chars.indexWhere(nonNumbers.contains(_))
    if (firstNonNumber > 0) {
      val (intSeq, rest) = chars.splitAt(firstNonNumber)
      val parsedInt = intSeq.mkString("").toInt
      (parsedInt, rest)
    } else if (firstNonNumber == 0) {
      throw new IllegalStateException("Trying to take int from non-integer starting array. This should not happen.")
    } else {
      val parsedInt = chars.mkString("").replace(",", "").toInt
      (parsedInt, Seq.empty)
    }
  }

  private def firstIntToArray(chars: Seq[Char]): Seq[Char] = {
    val (int, rest) = takeInt(chars)
    val wrapped = '[' +: int.toString.toCharArray :+ ']'
    wrapped ++ rest
  }

  @tailrec
  private def recurse(left: Seq[Char], right: Seq[Char]): Int = {
    (left.headOption, right.headOption) match {
      case (None, None) => 0
      case (None, Some(_)) => -1 // left side ran out of items
      case (Some(_), None) => 1 // right side ran out of items
      case (Some(','), Some(',')) => recurse(left.tail, right.tail)
      case (Some('['), Some('[')) => recurse(left.tail, right.tail) // both are arrays
      case (Some(']'), Some(']')) => recurse(left.tail, right.tail) // both were arrays of the same length
      case (Some('['), Some(']')) => 1 // right array ran out of items
      case (Some(']'), Some('[')) => -1 // left array ran out of items
      case (Some('['), Some(_)) =>
        recurse(left, firstIntToArray(right)) // left is an array and right is an integer, wrap right into array
      case (Some(_), Some('[')) =>
        recurse(firstIntToArray(left), right) // right is an array and left is an integer, wrap left into array
      case (Some(']'), Some(_)) => -1 // left array ran out of items
      case (Some(_), Some(']')) => 1 // right array ran out of items
      case (Some(_), Some(_)) => // both are integers
        val (leftInt, leftRest) = takeInt(left)
//        println(s"leftInt=$leftInt, rest=${leftRest.mkString("")}")
        val (rightInt, rightRest) = takeInt(right)
//        println(s"rightInt=$rightInt, rest=${rightRest.mkString("")}")
        leftInt.compare(rightInt) match {
          case 0 => recurse(leftRest, rightRest)
          case i if i > 0 => 1
          case _ => -1
        }
    }
  }

  def compareWith(right: String): Int =
    recurse(self.toCharArray.toSeq, right.toCharArray.toSeq)
}

val pairs = inputIterator
  .sliding(3, 3)
  .map(seq => Pair(seq.head, seq(1)))
  .toSeq
//println(pairs.mkString("\n"))

val compared = pairs
  .map(pair => {
    val res = pair.left.compareWith(pair.right)
//  println(s"comparing $pair -> $res")
    (pair, res)
  })
  .zipWithIndex

//println(compared.mkString("\n"))

val result1 = compared.filter { case ((pair, comparisonResult), index) => comparisonResult < 0 }.map(_._2 + 1).sum

val dividerPackets = Seq("[[2]]", "[[6]]")

val allPackets = pairs.flatMap(pair => Seq(pair.left, pair.right)) ++ dividerPackets

val sorted = allPackets.sortWith { case (left, right) => left.compareWith(right) < 0 }
val result2 = sorted.zipWithIndex.collect { case (packet, index) if dividerPackets.contains(packet) => index + 1 }.product

bufferedSource.close()
