package adventofcode

import adventofcode._
import org.scalatest.funsuite._
import adventofcode.day1.Day1
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import adventofcode.day1.Day2
import scala.util.matching.Regex

case class Position(horizontal: Int, depth: Int)

object Position {
  val start = Position(0,0)
}

class Day2Spec extends AnyWordSpec with Matchers with AdventOfCodeHelper{
  override def inputFilename(): String = "day2/input.txt"
  
  def solve(inputText: String) : Int = {

    var lines = inputText
      .split(System.lineSeparator())
      .filter(!_.isBlank())
      .map(_.trim())

    def next(currentPosition: Position, line: String) : Position = {
      val commmand = line.split(" ")(0)
      val ammount = line.split(" ")(1).toInt

      commmand match {
        case "down" => currentPosition.copy(depth = currentPosition.depth + ammount)
        case "up" => currentPosition.copy(depth = currentPosition.depth - ammount)
        case "forward" => currentPosition.copy(horizontal = currentPosition.horizontal + ammount)
      }
    }
    
    val finalPosition = lines.foldLeft(Position.start)(next)
    
    println(finalPosition)

    finalPosition.depth * finalPosition.horizontal
  }

  "part 1 example" in {
    var inputText = """forward 5
    down 5
    forward 8
    up 3
    down 8
    forward 2"""

    val answer = solve(inputText)
    println(answer)
    answer shouldBe (10 * 15)
  }

  "Part 1" in {
    val answer = solve(data)
    println(answer)
    answer shouldBe 2102357
  }
}
