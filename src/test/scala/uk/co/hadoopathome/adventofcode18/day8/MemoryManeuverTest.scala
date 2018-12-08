package uk.co.hadoopathome.adventofcode18.day8

import org.scalatest.FunSuite

import scala.io.Source

class MemoryManeuverTest extends FunSuite {
    test("sumMetadata test") {
        val input = Source.fromResource("day8/test-input.txt").mkString.split(" ").map(_.toInt).toList
        assert(138 === MemoryManeuver.sumMetadata(input))
    }

    test("sumMetadata real") {
        val input = Source.fromResource("day8/input.txt").mkString.split(" ").map(_.toInt).toList
        assert(45868 === MemoryManeuver.sumMetadata(input))
    }
}
