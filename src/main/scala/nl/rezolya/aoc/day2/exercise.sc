import nl.rezolya.aoc.day2.{Game, Paper, Rock}

import scala.io.Source

val day = 2

val filename = s"input/input$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

//val inputIterator = Seq(
//  "A Y",
//  "B X",
//  "C Z"
//).iterator

val parsed = inputIterator.map(Game.parse)
//println(parsed.map(_.shapeFor))
var result = parsed.map(_.score).sum

val x = Rock.play(Paper)

bufferedSource.close()
