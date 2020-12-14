package adventofcode.day1

object Day1 extends App {

  def solveText = textLinesToInt _ andThen solve _
  def solveText2 = textLinesToInt _ andThen solve2 _

  private def textLinesToInt(lines: String) : List[Int] = {
    lines
      .split(System.lineSeparator())
      .map(line => line.trim.toInt)
      .toList
  }

  def solve(expenses: List[Int]) : Int = {
    val targetExpense = 2020

    getMatch(expenses, targetExpense)
      .map(m => m._1 * m._2)
      .get
  }
  
  def solve2(expenses: List[Int]) : Int = {
    val targetExpense = 2020

    getMatch(expenses, targetExpense)
      .map(m => m._1 * m._2)
      .get
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
