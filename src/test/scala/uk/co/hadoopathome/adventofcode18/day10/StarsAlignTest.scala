package uk.co.hadoopathome.adventofcode18.day10

import org.scalatest.FunSuite
import uk.co.hadoopathome.adventofcode18.day10.StarsAlign.{Position, Velocity}

import scala.io.Source

/**
  * No assertions here - I found the correct answer by looking for the smallest value of Y, and then printing it
  */
class StarsAlignTest extends FunSuite {
    test("testMoveParticles test input") {
        val input = Source.fromResource("day10/test-input.txt").getLines().map(extractSteps).toList
        StarsAlign.moveParticles(input, 3)
    }

    test("testMoveParticles real") {
        val input = Source.fromResource("day10/input.txt").getLines().map(extractSteps).toList
        StarsAlign.moveParticles(input, 10711)
    }

    def extractSteps(s: String): (Position, Velocity) = {
        val letterPattern = "(-?\\d+)".r
        val numbers = letterPattern.findAllIn(s).map(x => x.toInt)
        ((numbers.next(), numbers.next()), (numbers.next(), numbers.next()))
    }
}
