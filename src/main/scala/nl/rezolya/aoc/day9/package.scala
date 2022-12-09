package nl.rezolya.aoc

package object day9 {

  case class Position(x: Int, y: Int) {

    def followTo(headPosition: Position): Position = {
//      println(s"\n$this to $headPosition - ")
      val newPosition =
        if (touches(headPosition)) {
//          println("touches")
          this
        } else {
          val xDif = x - headPosition.x
          val yDif = y - headPosition.y

          val sameColumn = !(Math.abs(xDif) > 0)
          val sameRow = !(Math.abs(yDif) > 0)
          (sameColumn, sameRow) match {
            case (false, false) => Position(newValue("x", xDif, x), newValue("y", yDif, y))
            case (false, true) => Position(newValue("x", xDif, x), y)
            case (true, false) => Position(x, newValue("y", yDif, y))
            case (true, true) => throw new Exception("error")
          }
        }

//      println(s" -> $newPosition")
      newPosition
    }

    def newValue(dimention: String, dif: Int, value: Int): Int = {
//      print(s"\n  $dimention Dif = $dif -> ")
      val newY = if (dif > 0) {
//        print(s"$dimention-1=${value - 1}")
        value - 1
      } else if (dif < 0) {
//        print(s"$dimention+1=${value + 1}")
        value + 1
      } else {
//        print(s"$dimention=$value")
        value
      }
//      println()
      newY
    }

    def touches(other: Position): Boolean = Math.abs(this.x - other.x) <= 1 && Math.abs(this.y - other.y) <= 1

    override def toString: String = s"($x,$y)"
  }

  sealed trait Direction {

    def move(currentPosition: Position, steps: Int): Position
    def moves(currentPosition: Position, steps: Int): Seq[Position] = {
      (1 to steps).map(ind => move(currentPosition, ind))
    }
  }

  case object Left extends Direction {
    override def move(currentPosition: Position, steps: Int): Position = {
      Position(currentPosition.x - steps, currentPosition.y)
    }
  }

  case object Right extends Direction {
    override def move(currentPosition: Position, steps: Int): Position = {
      Position(currentPosition.x + steps, currentPosition.y)
    }
  }
  case object Up extends Direction {
    override def move(currentPosition: Position, steps: Int): Position = {
      Position(currentPosition.x, currentPosition.y + steps)
    }
  }
  case object Down extends Direction {
    override def move(currentPosition: Position, steps: Int): Position = {
      Position(currentPosition.x, currentPosition.y - steps)
    }
  }

  object Move {
    def apply(str: String): Move = {
      val direction = str.head match {
        case 'L' => Left
        case 'R' => Right
        case 'U' => Up
        case 'D' => Down
      }
      val steps = str.drop(2).toInt
      Move(direction, steps)
    }
  }

  case class Move(direction: Direction, steps: Int) {

    def move(currentPosition: Position): Position = direction.move(currentPosition, steps)
    def moves(currentPosition: Position): Seq[Position] = direction.moves(currentPosition, steps)
  }

  case class Acc(currentPosition: Position, visited: Seq[Position])
}
