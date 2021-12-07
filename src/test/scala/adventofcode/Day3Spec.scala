package adventofcode

import adventofcode._
import org.scalatest.funsuite._
import adventofcode.day1.Day1
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import adventofcode.day1.Day2
import scala.util.matching.Regex
import java.math.BigInteger

class Day3Spec extends AnyWordSpec with Matchers with AdventOfCodeHelper{

  private val exampleInput = """
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""
  override def inputFilename(): String = "day3/input.txt"

  def solve(lines: Array[String]) : Int = {
    
    def digitValue(digitIndex: Int) : String = {
      val oneCount = lines.filter(line => line(digitIndex) == '1').length
      
      val zeroCount = lines.length - oneCount
    
      if (oneCount > zeroCount) "1" else "0"
    }

    val digitCount = lines.head.length()

    val gammaRateText = Seq.range(0, digitCount).map(digitValue).mkString
    val epsilonRateText = gammaRateText.map(digit => if (digit == '1') "0" else "1" ).mkString

    val gammaRate = new BigInteger(gammaRateText, 2).intValue()
    val epsilonRate = new BigInteger(epsilonRateText, 2).intValue()

    gammaRate * epsilonRate
  }

  "Part I" should {
    "Example" in {
      val lines = getLines(exampleInput).toArray
      println(solve(lines))
    }

    "Solution" in {

      val solution = solve(dataLines.toArray)

      println("Solution : " + solution)

      solution shouldBe 3813416
    }
  }
}
