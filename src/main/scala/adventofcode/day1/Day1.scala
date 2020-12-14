package adventofcode.day1

object Day1 extends App {
  def solve(lines: String) : Int = {
    
    val expenses = lines
      .split(System.lineSeparator())
      .map(line => line.trim.toInt)

    val m = getMatch(expenses.head, expenses.tail)

    m._1 * m._2
  }

  def getMatch(expense: Int, rest: Seq[Int]) : (Int, Int) = {
    rest
      .find(e => expense + e == 2020)
      .fold(getMatch(rest.head, rest.tail))( result => (expense, result) )
  }
}
