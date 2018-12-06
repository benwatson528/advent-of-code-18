package uk.co.hadoopathome.adventofcode18.day6

import org.scalatest.FunSuite

import scala.io.Source

class ChronalCoordinatesTest extends FunSuite {
    test("getLargestPointArea test input") {
        val input = parseInput("day6/test-input.txt")
        assert(17 === ChronalCoordinates.getLargestPointArea(input))
    }

    test("getLargestPointArea real") {
        val input = parseInput("day6/input.txt")
        assert(3894 === ChronalCoordinates.getLargestPointArea(input))
    }

    test("findSafeRegionSize test input") {
        val input = parseInput("day6/test-input.txt")
        assert(16 === ChronalCoordinates.findSafeRegionSize(input, 32))
    }

    test("findSafeRegionSize real") {
        val input = parseInput("day6/input.txt")
        assert(39398 === ChronalCoordinates.findSafeRegionSize(input, 10000))
    }

    def parseInput(path: String): List[(Int, Int)] =
        Source.fromResource(path).getLines().toList
            .map(x => (x.split(", ")(0).toInt, x.split(", ")(1).toInt))
}
