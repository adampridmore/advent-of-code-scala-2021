package adventofcode

import adventofcode._
import adventofcode.day1.Day1

class Day1Test extends org.scalatest.funsuite.AnyFunSuite {
  test("CubeCalculator.cube") {
    assert(Day1.cube(3) === 27)
  }
}
