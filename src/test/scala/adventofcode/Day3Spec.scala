package adventofcode

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
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
    
    def digitValue(digitIndex: Int) : Int = {
      val oneCount = lines.filter(line => line(digitIndex) == '1').length
      
      val zeroCount = lines.length - oneCount
    
      if (oneCount > zeroCount) 1 else 0
    }

    val digitCount = lines.head.length()

    val gammaRateDigits = Seq.range(0, digitCount).map(digitValue)
    val epsilonRateDigits = gammaRateDigits.map(digit => if (digit == 1) 0 else 1 )

    val gammaRate = new BigInteger(gammaRateDigits.mkString, 2).intValue()
    val epsilonRate = new BigInteger(epsilonRateDigits.mkString, 2).intValue()

    gammaRate * epsilonRate
  }

  "Part I" should {
    "Example" in {
      val lines = getLines(exampleInput).toArray
      val solution = solve(lines)
      // println(solve(lines))
      solution shouldBe 198
    }

    "Solution" in {
      val solution = solve(dataLines.toArray)
      // println("Solution : " + solution)
      solution shouldBe 3813416
    }
  }
}
