package uk.co.hadoopathome.adventofcode18.day7

import org.scalatest.FunSuite

import scala.io.Source

class PathFinderTest extends FunSuite {
  test("testFindPath 1 worker 0s test input") {
    val input = Source.fromResource("day7/test-input.txt").getLines().map(extractSteps).toList
    assert("CABDFE" === PathFinder.findPath(input, createWorkers(1, 0))._1)
  }

  test("testFindPath 1 worker 0s real") {
    val input = Source.fromResource("day7/input.txt").getLines().map(extractSteps).toList
    assert("HPDTNXYLOCGEQSIMABZKRUWVFJ" === PathFinder.findPath(input, createWorkers(1, 0))._1)
  }

  test("testFindPath 2 workers 0s test input") {
    val input = Source.fromResource("day7/test-input.txt").getLines().map(extractSteps).toList
    val pathAndTime = PathFinder.findPath(input, createWorkers(2, 0))
    assert("CABFDE" === pathAndTime._1)
    assert(15 === pathAndTime._2)
  }

  test("testFindPath 5 workers 60s real") {
    val input = Source.fromResource("day7/input.txt").getLines().map(extractSteps).toList
    val pathAndTime = PathFinder.findPath(input, createWorkers(5, 60))
    assert("HPXYDLOTCGNQSEMIAZBKRUWVFJ" === pathAndTime._1)
    assert(908 === pathAndTime._2)
  }

  def extractSteps(s: String): (Char, Char) = {
    val letterPattern = " [A-Z] ".r
    val letters = letterPattern.findAllIn(s).toList.map(x => x.toCharArray()(1))
    (letters(1), letters.head)
  }

  def createWorkers(numWorkers: Int, tickBase: Int): WorkerManager = {
    new WorkerManager(List.fill(numWorkers)('.', 0), tickBase)
  }
}
