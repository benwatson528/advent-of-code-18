package uk.co.hadoopathome.adventofcode18.day3

import org.scalatest.FunSuite

import scala.io.Source

class GridOwnerTest extends FunSuite {
    test("calculateGridCoords #1 @ 1,3: 4x4") {
        val input = GridInstruction(1, 1, 3, 2, 2)
        val output = CoordAndBoxId(Coord(1, 3), 1) ::
            CoordAndBoxId(Coord(1,4), 1) ::
            CoordAndBoxId(Coord(2, 3), 1) ::
            CoordAndBoxId(Coord(2, 4), 1) :: Nil
        assert(output === GridOwner.calculateGridCoords(input))
    }

    test("calculateNumberOverlapping test input") {
        val input = Source.fromResource("day3/test-input.txt").getLines.toList
            .map(convertToGridInstruction)
        assert(4 === GridOwner.findNumberOverlapping(input))
    }

    test("calculateNumberOverlapping real") {
        val input = Source.fromResource("day3/input.txt").getLines.toList
            .map(convertToGridInstruction)
        assert(100595 === GridOwner.findNumberOverlapping(input))
    }

    test("calculateNotOverlapping test input") {
        val input = Source.fromResource("day3/test-input.txt").getLines.toList
            .map(convertToGridInstruction)
        assert(3 === GridOwner.findNotOverlapped(input))
    }

    test("calculateNotOverlapping real") {
        val input = Source.fromResource("day3/input.txt").getLines.toList
            .map(convertToGridInstruction)
        assert(415 === GridOwner.findNotOverlapped(input))
    }

    def convertToGridInstruction(s: String): GridInstruction = {
        val split = s.split(' ')
        val boxId = split(0).substring(1).toInt
        val left = split(2).split(',')(0).toInt
        val top = split(2).split(',')(1).split(':')(0).toInt
        val width = split(3).split('x')(0).toInt
        val height = split(3).split('x')(1).toInt
        GridInstruction(boxId, left, top, width, height)
    }
}

