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
  
  "day 1 part I" should {
    "expenses example" should {
      "be 514579" in {
        val expenseLines = """1721
                              979
                              366
                              299
                              675
                              1456"""

        Day1.solveText(expenseLines) shouldBe Some(514579)
      }
    }

    "real file" in {
      val filename = "day1/expenses.txt"
      val lines = Source.fromResource(filename).getLines.map(_.toInt).toList
      println("Day 1 : Part I : " + Day1.solve(lines))
    }
  }

  "day 1 part II" should {
    "expenses example" should {
      "be 241861950" in {
        val expenseLines = """1721
    979
    366
    299
    675
    1456"""

        Day1.solveText2(expenseLines) shouldBe Some(241861950)
      }
    }

    "real file" in {
      val filename = "day1/expenses.txt"
      val lines = Source.fromResource(filename).getLines.map(_.toInt).toList
      println("Day 1 - Part II : " + Day1.solve2(lines))
    }

    "stuff" in {
      def tryParse(s: String) : Option[Int] = {
        val maybeInt = s.toIntOption

        def tryParseRomanNumeral(s: String) : Option[Int] = {
          s match {
            case "X" => Some(10) // Possibly doesn't convert all numerals...
            case _ => None
          }
        }

        maybeInt.orElse(tryParseRomanNumeral(s))
      }

      tryParse("1") shouldBe Some(1)
      tryParse("X") shouldBe Some(10)
    }
  }
}
