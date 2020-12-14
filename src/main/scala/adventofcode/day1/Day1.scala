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

  def solve(expenses: List[Int]) : Option[Int] = {
    val targetExpense = 2020

    getMatch(expenses, targetExpense)
      .map(m => m._1 * m._2)
  }
  
  def solve2(expenses: List[Int]) : Option[Int] = {
    
    val targetExpense = 2020

    def fn(expenses: List[Int]) : Option[(Int, Int, Int)] = {

      expenses match {
        case Nil => None
        case one::two::Nil => None
        case one::tail if (targetExpense - one > 0) => {
          getMatch(tail, targetExpense - one)
            .map({case (b,c) => (one, b,c)})
            .orElse(fn(expenses.tail))
        }
      }
    }

    fn(expenses)
      .map(r => (r._1 * r._2 * r._3) )
  }

  def getMatch(expenses: List[Int], target: Int) : Option[(Int, Int)] = {
    
    def tryMatchPair(head: Int, tail : List[Int]) : Option[(Int, Int)] = {
        tail
          .find(row => head + row == target)
          .fold(getMatch(tail,target))(result => Some(head, result) )
    }

    expenses match {
      case Nil => None
      case head::Nil => None
      case head::tail => tryMatchPair(head, tail)
    }
  }
}
