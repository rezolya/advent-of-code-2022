import nl.rezolya.aoc.day5._

import scala.io.Source

val day = 5

val filename = s"input/input$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

//val inputIterator = Seq(
//  "    [D]    ",
//  "[N] [C]    ",
//  "[Z] [M] [P]",
//  "1   2   3",
//  "",
//  "move 1 from 2 to 1",
//  "move 3 from 1 to 3",
//  "move 2 from 2 to 1",
//  "move 1 from 1 to 2"
//).iterator

val stacksStrings = inputIterator.takeWhile(s => !s.startsWith(" 1")).toList
inputIterator.next()
val movesStrings = inputIterator.toList

println(movesStrings.last)

val stacks = stacksStrings
  .map { str =>
    str.sliding(4, 4).map(s => Crate(s)).toList
  }
  .transpose
  .map { stack =>
    Stack(stack.filter(_.nonEmpty))
  }
  .zipWithIndex
  .map { case (stack, index) => index + 1 -> stack }
  .toMap

printStacks(stacks)

val moves = movesStrings.map(Move(_))

val head: Move = moves.head
head.execute(stacks)

val movedCrates = moves.foldLeft(stacks)((stack, move) => move.execute(stack))

printStacks(movedCrates)

val result = movedCrates.toSeq
  .sortBy(_._1)
  .map(_._2)
  .map(stack => stack.crates.head.letter.toString)
  .fold("")((acc, v) => acc + v)
bufferedSource.close()
