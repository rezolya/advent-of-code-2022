package nl.rezolya.aoc

import scala.annotation.tailrec

package object day14 {

  case class Position(x: Int, y: Int) {

    def down: Position = Position(x, y + 1)
    def downLeft: Position = Position(x - 1, y + 1)
    def downRight: Position = Position(x + 1, y + 1)
    def canDropTo: Seq[Position] = Seq(down, downLeft, downRight)
  }

  type Path = Seq[Position]

  class Cave(inputPaths: Seq[Path], inputPouringFrom: Position) {

    private val allPositions = inputPaths.flatten.toSet + inputPouringFrom
    private val minX = allPositions.map(_.x).min
    private val maxX = allPositions.map(_.x).max
    private val minY = allPositions.map(_.y).min
    private val maxY = allPositions.map(_.y).max

    val field: Array[Array[Char]] = Array.fill[Char](maxY - minY + 1, maxX - minX + 1)('.')
    val pouringFrom: Position = arrayPosition(inputPouringFrom)

    fillPaths()

    def printCave(): Unit = {
      for (row <- field.indices) {
        val r = field(row)
        println
        for (column <- r.indices) {
          print(r(column))
        }
      }
      println()
    }

    def countSand: Int = field.flatten.count(_ == 'o')

    @tailrec
    final def fillWithSand(): Unit = {
      val dropPath = dropSandUnitFrom(pouringFrom, Seq.empty)
      val finalPosition = dropPath.head
      if (!inAbyss(finalPosition)) {
        updateTile(finalPosition, 'o')
        fillWithSand()
      } else {
        dropPath.tail.foreach(p => updateTile(p, '~'))
      }
    }

    @tailrec
    private def dropSandUnitFrom(position: Position, pathSoFar: Seq[Position]): Seq[Position] = {
      canDropTo(position) match {
        case Some(nextPosition) if !inAbyss(nextPosition) => dropSandUnitFrom(nextPosition, nextPosition +: pathSoFar)
        case Some(abyssPosition) => abyssPosition +: pathSoFar
        case _ => pathSoFar
      }
    }

    private def canDropTo(position: Position): Option[Position] = {
      position.canDropTo.find(p => inAbyss(p) || !isBlocked(p))
    }

    private def fillPaths(): Unit = {
      updateTile(pouringFrom, '+')

      val adjustedPaths = inputPaths.map(_.map(arrayPosition))
      adjustedPaths.foreach { path =>
        val lines = path.sliding(2)
        lines.foreach(fillLine)
      }
    }

    private def fillLine(line: Seq[Position]): Unit = {
      line match {
        case Seq(start, end) if (start.x == end.x) =>
          val min = Math.min(start.y, end.y)
          val max = Math.max(start.y, end.y)
          for (y <- min to max) {
            updateTile(y, start.x, '#')
          }

        case Seq(start, end) if (start.y == end.y) =>
          val min = Math.min(start.x, end.x)
          val max = Math.max(start.x, end.x)
          for (x <- min to max) {
            updateTile(start.y, x, '#')
          }

        case _ => ()
      }
    }

    private def arrayPosition(position: Position): Position = Position(position.x - minX, position.y - minY)

    private def updateTile(position: Position, value: Char): Unit = {
      field(position.y)(position.x) = value
    }

    private def updateTile(y: Int, x: Int, value: Char): Unit = {
      field(y)(x) = value
    }

    private def isBlocked(position: Position): Boolean = {
      field(position.y)(position.x) == '#' || field(position.y)(position.x) == 'o'
    }

    private def inAbyss(position: Position): Boolean =
      position.x < 0 || position.x >= field.head.length || position.y < 0 || position.y >= field.length
  }

  class CaveWithFloor(inputField: Array[Array[Char]], inputPouringFrom: Position) {

    val field: Array[Array[Char]] =
      inputField :+ Array.fill(inputField.head.length)('.') :+ Array.fill(inputField.head.length)('#')
    var pouringFrom: Position = inputPouringFrom
    val growBy = 10

    def printCave(): Unit = {
      for (row <- field.indices) {
        val r = field(row)
        println
        for (column <- r.indices) {
          print(r(column))
        }
      }
      println()
    }

    def countSand: Int = field.flatten.count(_ == 'o')

    def growField(): Unit = {
//      println(s"growing by $growBy")
      for (row <- field.indices) {
        val r = field(row)
        val fillBy = if (row == field.length - 1) '#' else '.'
        val newR = Array.fill(growBy)(fillBy) ++ r ++ Array.fill(growBy)(fillBy)
        field(row) = newR
      }
      pouringFrom = Position(pouringFrom.x + growBy, pouringFrom.y)
//      printCave()
//      println()
    }

    @tailrec
    final def fillWithSand(): Unit = {
      val finalPosition = dropSandUnitFrom(pouringFrom)
      if (finalPosition != pouringFrom) {
        updateTile(finalPosition, 'o')

        if (closeToEdge(finalPosition)) {
          growField()
        }

        fillWithSand()
      } else {
        updateTile(finalPosition, 'o')
      }
    }

    @tailrec
    private def dropSandUnitFrom(position: Position): Position = {
      canDropTo(position) match {
        case Some(nextPosition) => dropSandUnitFrom(nextPosition)
        case _ => position
      }
    }

    private def canDropTo(position: Position): Option[Position] = {
      position.canDropTo.find(p => !isBlocked(p))
    }

    private def updateTile(position: Position, value: Char): Unit = {
      field(position.y)(position.x) = value
    }

    private def isBlocked(position: Position): Boolean = {
      field(position.y)(position.x) == '#' || field(position.y)(position.x) == 'o'
    }

    private def closeToEdge(position: Position): Boolean = {
      val dif = 5
      position.x < dif || position.x >= field.head.length - dif
    }
  }

}
