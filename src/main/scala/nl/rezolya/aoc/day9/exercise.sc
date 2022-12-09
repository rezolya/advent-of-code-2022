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

val moves = inputIterator.map(Move.apply)

val start: Position = Position(0, 0)

val headMovesAcc = moves.foldLeft(Acc(start, Seq(start))) { case (acc, move) =>
  val newMoves = move.moves(acc.currentPosition)
  val newPosition = newMoves.last
//  println(s"$newPosition: ${newMoves.mkString("->")} ")
  Acc(newPosition, acc.visited ++ newMoves)
}

val headMoves = headMovesAcc.visited

val tailMovesAcc = headMoves.foldLeft(Acc(start, Seq(start))) { case (acc, headPosition) =>
  val newPosition = acc.currentPosition.followTo(headPosition)
  Acc(newPosition, acc.visited :+ newPosition)
}

val tailMoves = tailMovesAcc.visited

//val x1 = Position(0,0).followTo(Position(0,0)) //(0,0)
//val x2 = Position(0,0).followTo(Position(1,0)) //(0,0)
//val x3 = Position(0,0).followTo(Position(1,1)) //(0,0)
//val x4 = Position(0,0).followTo(Position(2,0)) //(1,0)
//val x5 = Position(2,0).followTo(Position(0,0)) //(1,0)
//val x6 = Position(0,0).followTo(Position(0,2)) //(0,1)
//val x7 = Position(0,2).followTo(Position(0,0)) //(0,1)
//val x8 = Position(1,1).followTo(Position(2,3)) //(2,2)
//val x9 = Position(1,1).followTo(Position(3,2)) //(2,2)

val r = tailMoves.distinct.size

bufferedSource.close()
