package uk.co.hadoopathome.adventofcode18.day3

import org.scalatest.FunSuite
import uk.co.hadoopathome.adventofcode18.day3.GridOverlap.{Coord, GridInstruction}

import scala.io.Source

class GridOverlapTest extends FunSuite {
    test("calculateGridCoords #1 @ 1,3: 4x4") {
        val input = GridOverlap.GridInstruction(1, 1, 3, 2, 2)
        val output = Coord(1, 3) :: Coord(1,4) :: Coord(2, 3) :: Coord(2, 4) :: Nil
        assert(output === GridOverlap.calculateGridCoords(input))
    }

    test("calculateGridCoords test input") {
        val input = Source.fromResource("day3/test-input.txt").getLines.toList
            .map(convertToGridInstruction)
        assert(4 === GridOverlap.gridOverlap(input))
    }

    test("calculateGridCoords real") {
        val input = Source.fromResource("day3/input.txt").getLines.toList
            .map(convertToGridInstruction)
        assert(100595 === GridOverlap.gridOverlap(input))
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

