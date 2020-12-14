package adventofcode

import adventofcode._
import org.scalatest.funsuite._
import adventofcode.day1.Day1
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
// import org.scalatest.wordspec.WordSpec
// import org.scalatest.Matchers.should

class Day1Spec extends AnyWordSpec with Matchers {
  "expenses example" should {
    "be 514579" in {
      val expenseLines = """1721
  979
  366
  299
  675
  1456"""

      Day1.solve(expenseLines) shouldBe 514579
    }
  }

  "real file" in {
    val filename = "day1/expenses.txt"
    val lines = Source.fromResource(filename).getLines.toSeq
    println("Day 1 : " + Day1.solve(lines))
  }
}
