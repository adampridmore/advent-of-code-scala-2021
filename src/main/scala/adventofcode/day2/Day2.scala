package adventofcode.day1

import scala.util.matching.Regex

object Day2 extends App {

  private def countValidPasswords(isValid: (Int, Int, Char, String) => Boolean)(lines: Seq[String]) : Int = {
    val pattern = "([0-9]+)-([0-9]+) (\\w): (\\w+)".r

    lines
      .map(_ match {
          case pattern(a,b,c,d) => (a.toInt, b.toInt, c.toCharArray.head ,d)
        })
      .count( x => isValid(x._1, x._2, x._3, x._4) )
  }

  def solve : (Seq[String]) => Int = {  
    countValidPasswords((min: Int, max: Int, letter: Char, password: String) => {
      val count = password.count(c => c == letter)    
      count >= min && count <= max
    })
  }

  def solve2 : (Seq[String]) => Int = {
    countValidPasswords((i: Int, j: Int, letter: Char, password: String) => {
      (password(i-1) == letter, password(j-1) == letter) match {
        case (true, false) | (false, true) => true
        case _ => false
      }
    })
  }
}
