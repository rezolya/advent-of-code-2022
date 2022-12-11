import nl.rezolya.aoc.day11.Monkey

import scala.io.Source

val day = 11

val filename = s"input/input$day.txt"
//val filename = s"input/testInput$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

val monkeys = inputIterator.sliding(7, 7).map(Monkey.parse).toArray

def printItems(): Unit = {
  println(monkeys.map(m => s"Monkey ${m.nr}: ${m.items.mkString(", ")}").mkString("\n"))
}

val inspected = monkeys.map(_ => 0L)

val modulo = monkeys.map(_.testDivisibleBy).product

def doRound(i: Int): Unit = {
  for (monkey <- monkeys) {
    val value = monkey.turn(modulo)
    value
      .foreach { case (newLevel, throwTo) =>
        inspected(monkey.nr) = inspected(monkey.nr) + 1
        monkeys(throwTo).add(newLevel)
//        if(newLevel < 0){
//          throw new Exception(s"Overflow at round $i, monkey ${monkey}")
//        }
      }
  }
}


(1 to 10000).foreach { i =>
  doRound(i)
}

val result = inspected.sorted.reverse.take(2).product

bufferedSource.close()
