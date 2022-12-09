package nl.rezolya.aoc.day2

object GameResult{
  def parse(s: String): GameResult =
    s match {
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
    }
}

sealed trait GameResult {
  def score: Int
}

case object Win extends GameResult{
  val score = 6
}

case object Draw extends GameResult{
  val score = 3
}

case object Lose extends GameResult{
  val score = 0
}
