package adventofcode.day1

import scala.util.matching.Regex

object Day2 extends App {

  def solve(lines: Seq[String]) : Int = {  
    def isValid(min: Int, max: Int, letter: Char, password: String) : Boolean = {
      val passwordLetterCount = password.count(c => c == letter)
      
      passwordLetterCount >= min && passwordLetterCount <= max
    }

    val Pattern = "([0-9]+)-([0-9]+) (\\w): (\\w+)".r

    lines.map(line => {
        line match {
          case Pattern(a,b,c,d) => (a.toInt, b.toInt, c.toCharArray.head ,d)
        }
    })
    .count( x => isValid(x._1, x._2, x._3, x._4) )
  }

  def solve2(lines: Seq[String]) : Int = {
     def isValid(i: Int, j: Int, letter: Char, password: String) : Boolean = {
      (password(i-1) == letter, password(j-1) == letter) match {
        case (true, false) | (false, true) => true
        case _ => false
      }
    }

    val Pattern = "([0-9]+)-([0-9]+) (\\w): (\\w+)".r

    lines.map(line => {
        line match {
          case Pattern(a,b,c,d) => (a.toInt, b.toInt, c.toCharArray.head ,d)
        }
    })
    .count( x => isValid(x._1, x._2, x._3, x._4) )
  }
}
