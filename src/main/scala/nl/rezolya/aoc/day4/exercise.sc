import scala.io.Source

val day = 4

val filename = s"input/input$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

//val inputIterator = Seq(
//  "2-4,6-8",
//  "2-3,4-5",
//  "5-7,7-9",
//  "2-8,3-7",
//  "6-6,4-6",
//  "2-6,4-8"
//).iterator

val pattern = "(\\d*)-(\\d*),(\\d*)-(\\d*)".r

type Range = (Int, Int)
val parsed = inputIterator.map {
  case pattern(l1, r1, l2, r2) => ((l1.toInt, r1.toInt), (l2.toInt, r2.toInt))
}.toList

implicit class RangeOps(tuple: Range){
  def left = tuple._1
  def right = tuple._2
  def contains(other: Range): Boolean = tuple.left <= other.left && tuple.right >= other.right
  def overlaps(other: Range): Boolean =
    tuple.left <= other.left && tuple.right >= other.left ||  //l1 l2 r1
      tuple.left <= other.right && tuple.right >= other.right // l1 r2 r1
}

val containing = parsed.filter{ case (r1, r2) => r1.contains(r2) || r2.contains(r1) }
val result1= containing.size

val overlapping = parsed.count { case (r1, r2) => r1.overlaps(r2) || r2.overlaps(r1) }

bufferedSource.close()
