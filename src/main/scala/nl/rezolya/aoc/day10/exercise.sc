import scala.io.Source

val day = 10

val filename = s"input/input$day.txt"
//val filename = s"input/testInput$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

val input = inputIterator
//val input = Seq(
//  "addx 3",
//  "addx -5",
//  "noop"
//).iterator

val (finalValue, cycleValues) = input.foldLeft((1, Seq.empty[Int])) { case ((currentValue, cycleValues), op) =>
  op.take(4) match {
    case "noop" => (currentValue, cycleValues ++ Seq(currentValue))
    case "addx" => (currentValue + op.drop(5).toInt, cycleValues ++ Seq(currentValue, currentValue))
  }
}

val cycleValuesArray = cycleValues.toArray

val interestingIndexes = Seq(20, 60, 100, 140, 180, 220)

val signalStrength = interestingIndexes.map(i => i * cycleValuesArray(i - 1)).sum

val screenWidht = 40
val screenHeight = 6

var spritePositions = Seq(0, 1, 2)
val screen = Array.ofDim[Char](screenHeight, screenWidht)

var currentX = 0
var currentY = 0
for (cycle <- 1 until cycleValuesArray.length) {
  screen(currentY)(currentX) = if (spritePositions.contains(currentX)) '#' else '.'
  if (currentX < screenWidht - 1) {
    currentX = currentX + 1
  } else {
    currentX = 0
    currentY = currentY + 1
  }
  val currentValue = cycleValuesArray(cycle)
  spritePositions = Seq(currentValue - 1, currentValue, currentValue + 1)
}

def printScreen(): Unit = {
  for (y <- 0 until screenHeight) {
    for (x <- 0 until screenWidht) {
      print(screen(y)(x))
    }
    println
  }
}

printScreen()

bufferedSource.close()
