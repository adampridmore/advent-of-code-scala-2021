package adventofcode

import adventofcode._
import org.scalatest.funsuite._
import adventofcode.day1.Day1
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import adventofcode.day1.Day2
import scala.util.matching.Regex

class Day2Spec extends AnyWordSpec with Matchers {
  "day 2 example" in {
    val lines = """1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc""".split(System.lineSeparator()).toList

    Day2.solve(lines) shouldBe 2
  }

  "day 2" in {
    val lines = Source.fromResource("day2/passwords.txt").getLines().toSeq

    println("Day 2 part I : " + Day2.solve(lines))
  }
}
