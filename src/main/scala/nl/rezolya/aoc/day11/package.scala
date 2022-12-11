package nl.rezolya.aoc

package object day11 {

  type Level = Long

  object Monkey {
    def parse(strings: Seq[String]): Monkey = {
      val nr = strings(0).replace("Monkey ", "").dropRight(1).toInt
      val items = strings(1).replace("Starting items: ", "").split(',').map(x => x.trim.toLong).toList
      val operation = Operation.parse(strings(2).replace("Operation: new = ", "").trim)
      val testDivisibleBy = strings(3).replace("Test: divisible by ", "").trim.toInt
      val trueNr = strings(4).replace("If true: throw to monkey ", "").trim.toInt
      val falseNr = strings(5).replace("If false: throw to monkey ", "").trim.toInt
      Monkey(nr, items, operation, testDivisibleBy, trueNr, falseNr)
    }
  }

  case class Monkey(
    nr: Int,
    var items: List[Level],
    operation: Operation,
    testDivisibleBy: Int,
    trueNr: Int,
    falseNr: Int
  ) {

    def turn(modulo: Int): Seq[(Level, Int)] = {
//      println(s"Monkey $nr:")
      val throwsTo = items.map(turnItem(modulo, _))
      this.items = List.empty
      throwsTo
    }

    def turnItem(modulo: Int, level: Level): (Level, Int) = {
//      println(s"Monkey inspects an item with a worry level of $level.")
      val operationResult = operation.execute(level)
//      println(s"    Worry level is $operation to $operationResult.")

      val newLevel = operationResult % modulo /// 3
//      println(s"    Monkey gets bored with item. Worry level is divided by 3 to $newLevel.")
      val throwTo = if (newLevel % testDivisibleBy == 0) {
//        println(s"    Current worry level is not divisible by $testDivisibleBy.")
        trueNr
      } else {
//        println(s"    Current worry level is not divisible by $testDivisibleBy.")
        falseNr
      }
//      println(s"    Item with worry level $newLevel is thrown to monkey $throwTo.")
      (newLevel, throwTo)
    }
    def add(item: Level): Unit = this.items = this.items :+ item
  }

  object Operation {

    val pattern = "(.*) ([*+]) (.*)".r

    def parse(string: String): Operation = {
      string match {
        case pattern(left, op, right) => BinaryOperation(op, parseOperand(left), parseOperand(right))
      }
    }

    def parseOperand(string: String): Operation =
      if (string == "old") Old
      else Constant(string.toInt)
  }

  sealed trait Operation {
    def execute(input: Level): Level
  }

  case class BinaryOperation(operator: String, left: Operation, right: Operation) extends Operation {
    override def execute(input: Level): Level = operator match {
      case "*" => left.execute(input) * right.execute(input)
      case "+" => left.execute(input) + right.execute(input)
    }
  }

  case class Constant(value: Level) extends Operation {
    override def execute(input: Level): Level = value
  }

  case object Old extends Operation {
    override def execute(input: Level): Level = input
  }

}
