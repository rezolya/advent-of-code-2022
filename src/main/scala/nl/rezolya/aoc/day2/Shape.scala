package nl.rezolya.aoc.day2

object Shape {
  def parse(s: String): Shape =
    s match {
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
    }
}

sealed trait Shape {
  val itself: Shape = this
  val beats: Shape
  val losesTo: Shape
  def score: Int

  def play(shape: Shape): GameResult = {
    shape match {
      case `itself` => Draw
      case `beats` => Win
      case _ => Lose
    }
  }

  def shapeFor(gameResult: GameResult): Shape ={
    gameResult match {
      case Lose => beats
      case Draw => this
      case Win => losesTo
    }
  }
}

case object Rock extends Shape {
  val beats: Shape = Scissors
  val losesTo: Shape = Paper
  val score = 1
}

case object Scissors extends Shape {
  val beats: Shape = Paper
  val losesTo: Shape = Rock
  val score = 3
}

case object Paper extends Shape {
  val beats: Shape = Rock
  val losesTo: Shape = Scissors
  val score = 2
}

