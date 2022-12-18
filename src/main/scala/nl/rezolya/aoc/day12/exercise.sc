import nl.rezolya.aoc.day12._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.Queue
import scala.io.Source

val day = 12

val filename = s"input/input$day.txt"
//val filename = s"input/testInput$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[String] = bufferedSource.getLines()

val landscape: Array[Array[Char]] = inputIterator.map(_.toCharArray).toArray

val height = landscape.height
val width = landscape.width

val start = landscape.positionOf('S')
val end = landscape.positionOf('E')

updateLandscape(landscape, start, 'a')
updateLandscape(landscape, end, 'z')

@tailrec
def recurse(
  explorationQueue: Queue[Position],
  distances: Array[Array[Int]],
  hasFound: Position => Boolean,
  canVisit: (Position, Position) => Boolean
): Int = {
  explorationQueue.dequeueOption match {
    case Some((currentPosition, newQueue)) =>
      val currentDistance = distances(currentPosition.row)(currentPosition.column)
      if (hasFound(currentPosition)) {
        currentDistance
      } else {
        val stillToExplore = currentPosition
          .surrounding(landscape)
          .filter(p => canVisit(currentPosition, p) && distances(p.row)(p.column) == -1)
        stillToExplore.foreach { p =>
          updateDistance(distances, p, currentDistance + 1)
        }
        recurse(newQueue.enqueueAll(stillToExplore), distances, hasFound, canVisit)
      }
    case None => -1 // couldn't reach the end
  }
}

val fromStartToEnd = Array.fill(height, width)(-1)
updateDistance(fromStartToEnd, start, 0)

def canGoUp(current: Position, target: Position): Boolean = {
  val currentHeight = landscape(current.row)(current.column)
  val targetHeight = landscape(target.row)(target.column)
  val dif = targetHeight - currentHeight
  dif <= 1
}

val result1 = recurse(Queue(start), fromStartToEnd, _ == end, canGoUp)


val fromEndToA = Array.fill(height, width)(-1)
updateDistance(fromEndToA, end, 0)

def canGoDown(current: Position, target: Position): Boolean = {
  val currentHeight = landscape(current.row)(current.column)
  val targetHeight = landscape(target.row)(target.column)
  val dif = currentHeight - targetHeight
  dif <= 1
}

val result2 = recurse(Queue(end), fromEndToA, position => landscape(position.row)(position.column) == 'a', canGoDown)
