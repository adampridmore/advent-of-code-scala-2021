package adventofcode

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class Day4Spec extends AnyWordSpec with Matchers with AdventOfCodeHelper{

  private val exampleInput = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

"""

  type Square = Array[Array[Number]]
  
  trait Number{
    val number : Int
  }

  case class Marked(number :Int) extends Number
  case class Unmarked(number : Int) extends Number

  override def inputFilename(): String = "day4/input.txt"

  "Part I" should {
    "Example" in {
      val lines = getLines(exampleInput).toArray
      val drawnNumbers : Seq[Int] = lines.head.split(",").map(_.toInt)

      def parseSquare(lines : Array[String]) : Square = {
         def parseLine(line: String) : Array[Number] = {
            line.split(" ").map(number => number.toIntOption).flatten.map(Unmarked(_)).toArray
         }

        lines
          .map(parseLine)
          .toArray
      }

      var squares = 
          lines
            .sliding(6, 5)
            .map(block => parseSquare(block))
      
      def printSquare(square: Square) = { 
        println(square.map(_.map(n=>n.number).mkString(" ")).mkString(System.lineSeparator()))
        println()
      }

      squares.foreach(printSquare)
    }
  }
}
