package uk.co.hadoopathome.adventofcode18.day5

import org.scalatest.FunSuite

import scala.io.Source

class AlchemicalReductionTest extends FunSuite {
    test("reducePolymer aacbbc") {
        val input = "eaAdCbBce"
        assert("ede" === AlchemicalReduction.reducePolymer(input))
    }

    test("reducePolymer dabAcCaCBAcCcaDA") {
        val input = "dabAcCaCBAcCcaDA"
        assert("dabCBAcaDA" === AlchemicalReduction.reducePolymer(input))
        assert(10 === AlchemicalReduction.reducePolymer(input).length)
    }

    test("reducePolymer real") {
        val input = Source.fromResource("day5/input.txt").mkString
        assert(11242 === AlchemicalReduction.reducePolymer(input).length)
    }
}
