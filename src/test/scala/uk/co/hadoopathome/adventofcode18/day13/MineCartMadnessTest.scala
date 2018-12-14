package uk.co.hadoopathome.adventofcode18.day13

import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
import scala.io.Source

class MineCartMadnessTest extends FunSuite {
    val directions: List[Char] = '^' :: 'v' :: '<' :: '>' :: Nil

    test("runCarts test input") {
        val (board, carts) = parseInput("day13/part-1-test-input.txt")
        assert((7, 3) === MineCartMadness.runCarts(board, carts))
    }

    test("runCarts real") {
        val (board, carts) = parseInput("day13/input.txt")
        assert((43, 91) === MineCartMadness.runCarts(board, carts))
    }

    ignore("runCartsRemoveCrashed test input") {
        val (board, carts) = parseInput("day13/part-2-test-input.txt")
        assert((6, 4) === MineCartMadness.runCartsRemoveCrashed(board, carts))
    }

    test("runCartsRemoveCrashed real") {
        val (board, carts) = parseInput("day13/input.txt")
        assert((35, 59) === MineCartMadness.runCartsRemoveCrashed(board, carts))
    }

    private def parseInput(filename: String): (List[Track], List[Cart]) = {
        val input = Source.fromResource(filename).getLines().toList
        val rawBoard = new ListBuffer[Track]()

        val maxY = input.length
        for (j <- 0 until maxY) {
            val line = input(j)
            for (i <- 0 until line.length) {
                rawBoard += (((i, j), line.charAt(i)))
            }
        }
        val board = rawBoard.toList.filter(_._2 != ' ')
        val carts = getCarts(board)
        val replacedCartsBoard = replaceCarts(board)
        (replacedCartsBoard, carts)
    }

    def getCarts(board: List[((Int, Int), Char)]): List[Cart] = {
        val initCarts = board.filter(t => directions.contains(t._2))
        initCarts.map(c => Cart(c._1, getInitialDirection(c._2), TURN_LEFT))
    }

    def getInitialDirection(c: Char): Direction = {
        c match {
            case x if x == '^' => UP
            case x if x == 'v' => DOWN
            case x if x == '<' => LEFT
            case x if x == '>' => RIGHT
        }
    }

    def replaceCarts(board: List[Track]) =
        board.map(t => if (directions.contains(t._2)) replaceCart(t) else t)

    def replaceCart(t: Track): Track = {
        val newTrack = t._2 match {
            case x if x == '^' || x == 'v' => '|'
            case x if x == '<' || x == '>' => '-'
        }
        (t._1, newTrack)
    }
}
