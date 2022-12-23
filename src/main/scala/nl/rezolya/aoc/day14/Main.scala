package nl.rezolya.aoc.day14

import scala.io.Source

object Main extends App {

  val day = 14

  val filename = s"input/input$day.txt"
//  val filename = s"input/testInput$day.txt"
  val bufferedSource = Source.fromResource(filename)
  val inputIterator = bufferedSource.getLines()

  val paths: Seq[Path] = inputIterator.map { pathString =>
    pathString
      .split(" -> ")
      .map { pair =>
        val strings = pair.split(",")
        Position(strings.head.toInt, strings(1).toInt)
      }
      .toSeq
  }.toSeq

  bufferedSource.close()

  val pouringFrom = Position(500, 0)

  val cave = new Cave(paths, pouringFrom)

  val rockField = cave.field.map(_.map(identity))

  println("unfilled cave:")
  cave.printCave()

  cave.fillWithSand()

  println("\nfilled cave:")
  cave.printCave()

  val result1 = cave.countSand
  println(s"result1 = $result1")


  val caveWithFloor = new CaveWithFloor(rockField, cave.pouringFrom)

  println("unfilled cave with floor:")
  caveWithFloor.printCave()

  caveWithFloor.fillWithSand()

  println("\nfilled cave with floor:")
  caveWithFloor.printCave()

  val result2= caveWithFloor.countSand
  println(s"result2 = $result2")

}
