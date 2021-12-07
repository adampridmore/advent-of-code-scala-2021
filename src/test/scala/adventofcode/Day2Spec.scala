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

case class PositionPart2(horizontal: Int, depth: Int, aim: Int)

object PositionPart2 {
  val start = PositionPart2(0,0,0)
}

class Day2Spec extends AnyWordSpec with Matchers with AdventOfCodeHelper{
  override def inputFilename(): String = "day2/input.txt"
  case class Command(text: String, ammount: Int)

  def getCommands(inputText : String) : Seq[Command] = {
    inputText
      .split(System.lineSeparator())
      .filter(!_.isBlank())
      .map(_.trim())
      .map(line => {
        val bits = line.split(" ")
        Command(bits(0), bits(1).toInt)
      })
  }
   
  def solve[T](inputText: String)(zero: T)(next: (T, Command) => T) : T = {
    getCommands(inputText)
      .foldLeft(zero)(next)
  }

  "part 1" should {
    def solvePart1(inputText: String) : Int = {

      def next(currentPosition: Position, command: Command) : Position = {
        (command.text) match {
          case "down" => currentPosition.copy(depth = currentPosition.depth + command.ammount)
          case "up" => currentPosition.copy(depth = currentPosition.depth - command.ammount)
          case "forward" => currentPosition.copy(horizontal = currentPosition.horizontal + command.ammount)
        }
      }
    
      val finalPosition = solve(inputText)(Position.start)(next)
    
      finalPosition.depth * finalPosition.horizontal
    }

    "example" in {
      var inputText = """forward 5
      down 5
      forward 8
      up 3
      down 8
      forward 2"""

      val answer = solvePart1(inputText)
      // println(answer)
      answer shouldBe (10 * 15)
    }

    "Solution" in {
      val answer = solvePart1(data)
      // println(answer)
      answer shouldBe 2102357
    }
  }
  
  "part 2" should {
    def solvePart2(inputText: String) : Int = {

      def next(currentPosition: PositionPart2, command: Command) : PositionPart2 = {
        (command.text) match {
          case "down" => currentPosition.copy(aim = currentPosition.aim + command.ammount)
          case "up" => currentPosition.copy(aim = currentPosition.aim - command.ammount)
          case "forward" => {
            currentPosition.copy(
              horizontal = currentPosition.horizontal + command.ammount,
              depth = currentPosition.depth + (currentPosition.aim*command.ammount))
          }
        }
      }
      
      val finalPosition = solve(inputText)(PositionPart2.start)(next)
    
      finalPosition.depth * finalPosition.horizontal
    }

    "example" in {
      var inputText = """forward 5
      down 5
      forward 8
      up 3
      down 8
      forward 2"""

      val answer = solvePart2(inputText)
      // println(answer)
      answer shouldBe (15 * 60)
    }

    "Solution" in {
      val answer = solvePart2(data)
      // println(answer)
      answer shouldBe 2101031224
    }
  }
}
