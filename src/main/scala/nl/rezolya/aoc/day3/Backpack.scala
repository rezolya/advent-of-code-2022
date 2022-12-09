package nl.rezolya.aoc.day3

import scala.annotation.tailrec

object Backpack{
  def apply(itemsString: String): Backpack = {
    val array = itemsString.toCharArray
    val middle = array.size / 2
    val (left, right) = array.splitAt(middle)
    Backpack(left.toSeq.map(Item), right.toSeq.map(Item))
  }
}

case class Backpack(leftCompartment: Seq[Item], rightCompartment: Seq[Item]){
  def inBoth: Item = leftCompartment.find { item =>
    rightCompartment.contains(item)
  }.getOrElse(throw new Exception(s"No matching items in backpack '$this'"))

  def allItems: Seq[Item] = leftCompartment ++ rightCompartment
  def contains(item: Item): Boolean = allItems.contains(item)
}

case class Group(backpacks: Seq[Backpack]){
  @tailrec
  private def recurse(items: Seq[Item], backpacks: Seq[Backpack]): Seq[Item] = {
    backpacks match {
      case head :: tail =>
        val filtered = items.filter(item => head.contains(item))
        recurse(filtered, tail)
      case Nil => items
    }
  }
  def badge: Item = {
    recurse(backpacks.head.allItems, backpacks.tail).head
  }
}

case class Item(letter: Char) {
  def priority: Int = if (letter.isUpper) {
    letter.toInt - 64 + 26
  } else if (letter.isLower) {
    letter.toInt - 96
  } else throw new Exception(s"Invalid item type '$letter'")

  override def toString: String = letter.toString
}
