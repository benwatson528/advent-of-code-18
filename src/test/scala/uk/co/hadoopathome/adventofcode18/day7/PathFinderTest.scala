package uk.co.hadoopathome.adventofcode18.day7

import org.scalatest.FunSuite

import scala.io.Source

class PathFinderTest extends FunSuite {

  test("testFindPath test input") {
    val input = Source.fromResource("day7/test-input.txt").getLines().map(extractSteps).toList
    assert("CABDFE" === PathFinder.findPath(input))
  }

  test("testFindPath real") {
    val input = Source.fromResource("day7/input.txt").getLines().map(extractSteps).toList
    assert("HPDTNXYLOCGEQSIMABZKRUWVFJ" === PathFinder.findPath(input))
  }

  def extractSteps(s: String): (Char, Char) = {
    val letterPattern = " [A-Z] ".r
    val letters = letterPattern.findAllIn(s).toList.map(x => x.toCharArray()(1))
    (letters(1), letters.head)
  }
}
