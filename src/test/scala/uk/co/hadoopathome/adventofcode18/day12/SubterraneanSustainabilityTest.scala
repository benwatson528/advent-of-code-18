package uk.co.hadoopathome.adventofcode18.day12

import org.scalatest.FunSuite
import uk.co.hadoopathome.adventofcode18.day12.SubterraneanSustainability.{Pots, Rules}

import scala.io.Source

class SubterraneanSustainabilityTest extends FunSuite {
    test("processRecords 20 iterations test input") {
        val (pots, rules) = parseInput("day12/test-input.txt")
        assert(325 === getSum(SubterraneanSustainability.processRecords(pots, rules, 20)))
    }

    test("processRecords 20 iterations real input") {
        val (pots, rules) = parseInput("day12/input.txt")
        assert(3221 === getSum(SubterraneanSustainability.processRecords(pots, rules, 20)))
    }

    test("processRecords 50000000000 iterations real input") {
        val (pots, rules) = parseInput("day12/input.txt")
        val resultsAfterHundred = SubterraneanSustainability.processRecords(pots, rules, 1000)
        val incrementAmount = 50000000000l-1000l
        val score = resultsAfterHundred.filter(_._1 == '#').map(_._2 + incrementAmount).sum
        assert(2600000001872l === score)
    }

    def parseInput(filename: String): (Pots, Rules) = {
        val rawInput = Source.fromResource(filename).getLines().toList
        val pots = rawInput.head.split(": ")(1).zipWithIndex.toList
        val rules = rawInput.tail.tail.map(x => (x.split(" => ")(0), x.split(" => ")(1)))
            .filter(_._2 == "#").map(_._1).toSet
        (pots, rules)
    }

    def getSum(pots: Pots): Int = pots.filter(_._1 == '#').map(_._2).sum
}
