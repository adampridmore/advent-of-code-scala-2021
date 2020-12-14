package adventofcode

import adventofcode._
import org.scalatest.funsuite._
import adventofcode.day1.Day1
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
// import org.scalatest.wordspec.WordSpec
// import org.scalatest.Matchers.should

class Day1Spec extends AnyWordSpec with Matchers {
  "expenses" should {
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
}
