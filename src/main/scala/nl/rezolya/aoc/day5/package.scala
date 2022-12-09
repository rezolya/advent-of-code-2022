package nl.rezolya.aoc

import scala.util.matching.Regex

package object day5 {

  def printStacks(stacks: Map[Int, Stack]): Unit = for (i <- 1 to stacks.size) {
    println(s"$i: ${stacks(i)}")
  }

  object Crate {

    def apply(str: String): Crate = if (str.isBlank) {
      Crate.empty
    } else {
      val letter = str.toCharArray.apply(1)
      Crate(letter)
    }

    def empty: Crate = Crate('-')
  }

  case class Crate(letter: Char) {

    override def toString: String = s"[$letter]"
    def nonEmpty: Boolean = letter != '-'
  }

  case class Stack(crates: List[Crate]) {

    override def toString: String = crates.mkString(" - ")

    def take(amount: Int): (Stack, List[Crate]) = {
      val (taken, left) = crates.splitAt(amount)
      Stack(left) -> taken
    }

    def add(toAdd: List[Crate]): Stack = Stack(toAdd ++ crates)
  }

  object Move {

    val pattern: Regex = "move (.*) from (.*) to (.*)".r
    def apply(str: String): Move = {
      str match {
        case pattern(amount, from, to) =>
          Move(amount.toInt, from.toInt, to.toInt)
      }
    }
  }

  case class Move(amount: Int, from: Int, to: Int) {

    override def toString: String = s"move: $amount from $from to $to"

    def execute(stacks: Map[Int, Stack]): Map[Int, Stack] = {
      val fromStack = stacks(from)
      val (newFrom, taken) = fromStack.take(amount)
      val newTo = stacks(to).add(taken)

      stacks
        .updated(from, newFrom)
        .updated(to, newTo)
    }
  }
}
