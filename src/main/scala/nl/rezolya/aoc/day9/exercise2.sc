import nl.rezolya.aoc.day9._
import scala.io.Source

val day = 9

val filename = s"input/input$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

//val inputIterator = Seq(
//  "R 4",
//  "U 4",
//  "L 3",
//  "D 1",
//  "R 4",
//  "D 1",
//  "L 5",
//  "R 2"
//).iterator

//val inputIterator = Seq(
//  "R 5",
//  "U 8",
//  "L 8",
//  "D 3",
//  "R 17",
//  "D 10",
//  "L 25",
//  "U 20"
//).iterator

val moves = inputIterator.map(Move.apply)

val ropeLenght = 10
val height = 25
val widht = 25
val positions = Array.ofDim[Position](ropeLenght)

val start: Position = Position(0, 0)

for (i <- 0 until ropeLenght) {
  positions(i) = start
}

def printPositions(): Unit = {
  for (y1 <- 0 until height) {
    val y = height - y1 - 1
    for (x <- 0 until widht) {
      var printed = false
      var e = 0
      while (e < ropeLenght && !printed) {
        val current = positions(e)
        if (current.x == x && current.y == y) {
          print(s" ${if (e == 0) "H" else e}")
          printed = true
        }
        e = e + 1
      }
      if (!printed) {
        print(s" .")
      }
    }
    println()
  }
}

var tailPositions = Seq[Position]()

moves.foreach { move =>
//  println(s"\n == $move ==\n\n")

  val headPosition = positions(0)
  val newSteps = move.moves(headPosition)
  newSteps.foreach { newHead =>
    positions(0) = newHead

    for (x <- 1 until ropeLenght) {
      val currentTail = positions(x)
      val newTail = currentTail.followTo(positions(x - 1))
      positions(x) = newTail

      if (x == ropeLenght - 1) {
        tailPositions = tailPositions :+ newTail
      }
    }

//    printPositions()
//    println()
  }
}

val r = tailPositions.distinct.size

bufferedSource.close()
