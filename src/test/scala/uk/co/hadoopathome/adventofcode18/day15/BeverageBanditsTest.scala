package uk.co.hadoopathome.adventofcode18.day15

import org.scalatest.FunSuite

import scala.io.Source

class BeverageBanditsTest extends FunSuite {
    test("testRunRPG test 1") {
        val input = parseInput("day15/test-1-input.txt")
    }

    def parseInput(filename: String): (List[Elf], List[Goblin], Set[Coord]) = {
        val input = Source.fromResource(filename).getLines().toList
        var board = Set[Coord]()
        var elves = List[Elf]()
        var goblins = List[Goblin]()

        val maxY = input.length
        for (j <- 0 until maxY) {
            val line = input(j)
            for (i <- 0 until line.length) {
                if (line(i) == '.') board = board + Coord(i, j)
                else if (line(i) == 'G') goblins = goblins :+ Goblin(Coord(i, j), 3, 200)
                else if (line(i) == 'E') elves = elves :+ Elf(Coord(i, j), 3, 200)
            }
        }
        (elves, goblins, board)
    }
}
