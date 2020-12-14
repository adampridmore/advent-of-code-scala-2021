package adventofcode.day1

object Day1 extends App {
  def solve1(expenses: List[Int]) : Int = {
    val targetExpense = 2020

    getMatch(expenses, targetExpense)
      .map(m => m._1 * m._2)
      .get
  }

  def solve1(lines: String) : Int = {
    solve1(lines
      .split(System.lineSeparator())
      .map(line => line.trim.toInt)
      .toList)
  }

  def getMatch(expenses: List[Int], target: Int) : Option[(Int, Int)] = {

    println(s"e: ${expenses.mkString(",")} target: $target")

    expenses match {
      case head::Nil => None
      case head::tail => {
        tail
          .find(row => head + row == target)
          .fold(getMatch(tail,target))(result => Some(head, result) )
      }
      case Nil => None
    }
  }
}
