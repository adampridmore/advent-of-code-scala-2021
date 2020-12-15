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
    (for {
      a <- expenses
      b <- expenses.tail
    } yield(a,b))
      .find(x => x._1 + x._2 == 2020)
      .map(x => x._1 * x._2)
  }
  
  def solve2(expenses: List[Int]) : Option[Int] = {
    
    (for {
      a <- expenses
      b <- expenses.tail
      if a + b < 2020
      c <- expenses.tail.tail
    } yield(a,b,c))
      .find(x => x._1 + x._2 + x._3 == 2020)
      .map(x => x._1 * x._2 * x._3)
  }
}
