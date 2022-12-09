import scala.io.Source

val day = 1

val filename = s"input/input$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

var combined = LazyList[List[Int]]()

while(inputIterator.hasNext){
  val group = inputIterator.takeWhile(!_.isBlank).map(_.toInt).toList
  combined = combined :+ group
}

val sums = combined.map(_.sum)
val max = sums.max

println(s"max = $max")

val orderedSums = sums.sorted.reverse
val top3 = orderedSums.take(3)

println(s"top 3: ${top3.force.sum}")

bufferedSource.close()
