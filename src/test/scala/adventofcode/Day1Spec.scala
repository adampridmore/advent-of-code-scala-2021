package adventofcode

import adventofcode._
import org.scalatest.funsuite._
import adventofcode.day1.Day1
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

class Day1Spec extends AnyWordSpec with Matchers {

  private def textToInts(lines: String) : Seq[Int] = {
    lines
      .split(System.lineSeparator())
      .map(line => line.trim)
      .filter(line => line.nonEmpty)
      .map(_.toInt)
      .toList
  }

  val exampleText : String = """
199
200
208
210
200
207
240
269
260
263
"""
  val exampleInts : Seq[Int] = textToInts(exampleText)

  val filename = "day1/input.txt"
  val data = Source
      .fromResource(filename)
      .getLines
      .map(_.toInt)
      .toSeq

  def solveA(ints : Seq[Int]) : Int = {
    ints
      .zip(ints.tail)
      .map({case (a, b) => a<b})
      .filter(x=>x)
      .length
  }
  
  def solveB(ints : Seq[Int]) : Int = { 
    val windows = 
        ints
        .zip(ints.tail)
        .zip(ints.tail.tail)
        .map{case ((a,b),c) => a+b+c}
    
      solveA(windows)
  }


  def assertAndPrint[T](message: String, answer: T, expected: T) = {
    println(s"${message.padTo(30, ' ')} - $answer")
    answer shouldBe expected
  }
  
  "Day 1" should {
    "Part 1 Example" in {
      assertAndPrint("Part 1 example", solveA(exampleInts), 7)
    }

    "Part 1" in {
      assertAndPrint("Part 1", solveA(data), 1564)
    }

    "Part 2 Example" in {
      assertAndPrint("Part 2 Example", solveB(exampleInts), 5)
    }

    "Part 2" in {
      assertAndPrint("Part 2", solveB(data), 1611)
    }
  }
}
