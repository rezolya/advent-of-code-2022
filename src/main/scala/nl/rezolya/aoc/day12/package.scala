package nl.rezolya.aoc

package object day12 {

  case class Position(row: Int, column: Int) {

    def surrounding(landscape: Array[Array[Char]]): Seq[Position] = Seq(
      if (row + 1 < landscape.height) Some(Position(row + 1, column)) else None,
      if (row - 1 >= 0) Some(Position(row - 1, column)) else None,
      if (column + 1 < landscape.width) Some(Position(row, column + 1)) else None,
      if (column - 1 >= 0) Some(Position(row, column - 1)) else None
    ).flatten

    override def toString: String = s"($row,$column)"
  }

  implicit class MatrixOps[T](matrix: Array[Array[T]]) {

    def height: Int = matrix.length
    def width: Int = matrix.head.length

    def positionOf(value: T): Position = {
      val row = matrix.indexWhere(_.contains(value))
      val column = matrix(row).indexOf(value)
      Position(row, column)
    }

    def printMatrix(withIndex: Boolean = false): Unit = {
      for (row <- matrix.indices) {
        val r = matrix(row)
        if (withIndex)
          print(s"\n$row:")
        else println
        for (column <- r.indices) {
          if (withIndex) print(s" $column:")
          print(r(column))
        }
      }
    }
  }

  def updateLandscape(matrix: Array[Array[Char]], position: Position, value: Char): Unit = {
    matrix(position.row).update(position.column, value)
  }
  def updateDistance(matrix: Array[Array[Int]], position: Position, value: Int): Unit = {
    matrix(position.row).update(position.column, value)
  }

}
