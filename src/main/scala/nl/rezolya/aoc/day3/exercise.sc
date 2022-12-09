import nl.rezolya.aoc.day3.{Backpack, Group, Item}

import scala.io.Source

val day = 3

val filename = s"input/input$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

//val inputIterator = Seq(
//  "vJrwpWtwJgWrhcsFMMfFFhFp",
//  "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
//  "PmmdzqPrVvPwwTWBwg",
//  "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
//  "ttgJtRGJQctTZtZT",
//  "CrZsJsPPZsGzwwsLwLmpwMDw",
//).iterator

val a = Seq('a','z','A','Z')
println(a.map(Item).map(_.priority))

val parsed = inputIterator.map(Backpack(_)).toList

val result = parsed.map(_.inBoth).map(_.priority).sum
println(result)

var combined = LazyList[Group]()
val parsedIterator = parsed.iterator
while(parsedIterator.hasNext){
  val group = Group(parsedIterator.take(3).toSeq)
  combined = combined :+ group
}

val result2 = combined.map(_.badge).map(_.priority).sum
println(result2)

bufferedSource.close()
