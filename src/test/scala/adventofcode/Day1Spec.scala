package adventofcode

import adventofcode._
import org.scalatest.funsuite._
import adventofcode.day1.Day1
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

class Day1Spec extends AnyWordSpec with Matchers {
  "getMatch" should {
    "No items" in {
      Day1.getMatch(List.empty, 100) shouldBe None
    }

    "One item" in {
      Day1.getMatch(List(1), 100) shouldBe None
    }

    "Two items that match" in {
      Day1.getMatch(List(25, 75), 100) shouldBe Some(25, 75)
    }

    "Two items that dont match" in {
      Day1.getMatch(List(25, 75),200) shouldBe None
    }
  }
  
  "day 1" should {

    "expenses example" should {
      "be 514579" in {
        val expenseLines = """1721
                              979
                              366
                              299
                              675
                              1456"""

        Day1.solveText(expenseLines) shouldBe 514579
      }
    }

    "real file" ignore {
      val filename = "day1/expenses.txt"
      val lines = Source.fromResource(filename).getLines.map(_.toInt).toList
      println("Day 1 : " + Day1.solve(lines))
    }
  }

  "day 1 part II" should {
    "expenses example" should {
      "be 514579" in {
        val expenseLines = """1721
    979
    366
    299
    675
    1456"""

        Day1.solveText2(expenseLines) shouldBe 437931
      }
    }
  }
}
