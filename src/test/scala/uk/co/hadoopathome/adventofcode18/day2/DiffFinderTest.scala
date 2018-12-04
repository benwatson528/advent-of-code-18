package uk.co.hadoopathome.adventofcode18.day2

import org.scalatest.FunSuite

import scala.io.Source

class DiffFinderTest extends FunSuite {
    test("compareWords similar") {
        val word1 = "fghij".toList
        val word2 = "fguij".toList
        assert("fgij" === DiffFinder.compareWords(word1, word2).get.mkString)
    }

    test("compareWords dissimilar") {
        val word1 = "abcde".toList
        val word2 = "axcye".toList
        assert(None === DiffFinder.compareWords(word1, word2))
    }

    test("compareWords different lengths") {
        val word1 = "abcde".toList
        val word2 = "abcdef".toList
        assert(None === DiffFinder.compareWords(word1, word2))
    }

    test("diffFinder test input") {
        val input = Source.fromResource("day2/part-2-test-input.txt").getLines.toList
            .map(_.toCharArray.toList)
        assert("fgij" === DiffFinder.diffFinder(input).mkString)
    }

    test("diffFinder real") {
        val input = Source.fromResource("day2/input.txt").getLines.toList
            .map(_.toCharArray.toList)
        assert("wrziyfdmlumeqvaatbiosngkc" === DiffFinder.diffFinder(input).mkString)
    }
}
