package adventofcode.day1

object Day1 extends App {
  def solve1(lines: Seq[String]) : Int = {
  val expenses = lines.map(line => line.trim.toInt)

    val m = getMatch(expenses.head, expenses.tail)

    m._1 * m._2
  }

  def solve1(lines: String) : Int =  solve1(lines.split(System.lineSeparator()))

  def getMatch(expense: Int, rest: Seq[Int]) : (Int, Int) = {
    rest
      .find(e => expense + e == 2020)
      .fold(getMatch(rest.head, rest.tail))( result => (expense, result) )
  }
}
