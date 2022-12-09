package nl.rezolya.aoc.day2

import scala.util.matching.Regex

object Game {
  val pattern: Regex = "(.) (.)".r
  def parse(string: String): Game = {
    string match {
      case pattern(first, second) => Game(Shape.parse(first), GameResult.parse(second))
      case _ => throw new Exception(s"Couldn't parse game: \"$string\"")
    }
  }
}

case class Game(opponentShape: Shape, result: GameResult) {
  //  def play: GameResult = yourShape.play(opponentShape)
  def shapeFor: Shape = opponentShape.shapeFor(result)
  def score: Int = shapeFor.score + result.score
}

