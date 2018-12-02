package uk.co.hadoopathome.adventofcode18.day2

import org.scalatest.FunSuite

import scala.io.Source

class InventoryManagementTest extends FunSuite {
    test("findOccurrencesInString abcdef") {
        val input = "abcdef".toList
        assert(false === InventoryManagement.hasOccurrencesInString(input, 2))
        assert(false === InventoryManagement.hasOccurrencesInString(input, 3))
    }

    test("findOccurrencesInString bababc") {
        val input = "bababc".toList
        assert(true === InventoryManagement.hasOccurrencesInString(input, 2))
        assert(true === InventoryManagement.hasOccurrencesInString(input, 3))
    }

    test("findOccurrencesInString abbcde") {
        val input = "abbcde".toList
        assert(true === InventoryManagement.hasOccurrencesInString(input, 2))
        assert(false === InventoryManagement.hasOccurrencesInString(input, 3))
    }

    test("findOccurrencesInString abcccd") {
        val input = "abcccd".toList
        assert(false === InventoryManagement.hasOccurrencesInString(input, 2))
        assert(true === InventoryManagement.hasOccurrencesInString(input, 3))
    }

    test("findOccurrencesInString aabcdd") {
        val input = "aabcdd".toList
        assert(true === InventoryManagement.hasOccurrencesInString(input, 2))
        assert(false === InventoryManagement.hasOccurrencesInString(input, 3))
    }

    test("generateChecksum testInput") {
        val input = Source.fromResource("day2/testInput.txt").getLines.toList.map(_.toCharArray.toList)
        assert(12 === InventoryManagement.generateChecksum(input))
    }

    test("generateChecksum real") {
        val input = Source.fromResource("day2/input.txt").getLines.toList.map(_.toCharArray.toList)
        assert(4940 === InventoryManagement.generateChecksum(input))
    }
}
