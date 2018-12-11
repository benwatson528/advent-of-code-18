package uk.co.hadoopathome.adventofcode18.day9

import org.scalatest.FunSuite

class MarbleManiaTest extends FunSuite {
  test("testAddPiece add to end") {
    val board = (3, 16 :: 17 :: 18 :: 19 :: 20 :: Nil)
    assert((5, 16 :: 17 :: 18 :: 19 :: 20 :: 21 :: Nil) === MarbleMania.addPieceNormal(board, 21))
  }

  test("testAddPiece wrap around") {
    val board = (4, 16 :: 17 :: 18 :: 19 :: 20 :: Nil)
    assert((1, 16 :: 21 :: 17 :: 18 :: 19 :: 20 :: Nil) === MarbleMania.addPieceNormal(board, 21))
  }

  test("testAddPiece mid insert") {
    val board = (1, 16 :: 17 :: 18 :: 19 :: 20 :: Nil)
    assert((3, 16 :: 17 :: 18 :: 21 :: 19 :: 20 :: Nil) === MarbleMania.addPieceNormal(board, 21))
  }

  test("testAddPieceSpecial mid insert") {
    val board = (10, 10 :: 11 :: 12 :: 13 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: 20 :: 21 :: 22 :: Nil)
    assert(((3, 10 :: 11 :: 12 :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: 20 :: 21 :: 22 :: Nil), 36) === MarbleMania.addPieceSpecial(board, 23))
  }

  test("testAddPieceSpecial wrap around") {
    val board = (4, 10 :: 11 :: 12 :: 13 :: 14 :: 15 :: Nil)
    assert(((3, 10 :: 11 :: 12 :: 14 :: 15 :: Nil), 36) === MarbleMania.addPieceSpecial(board, 23))
  }

  test("testAddPieceSpecial last element") {
    val board = (0, 10 :: 11 :: 12 :: 13 :: 14 :: 15 :: Nil)
    assert(((0, 10 :: 11 :: 12 :: 13 :: 14 :: Nil), 38) === MarbleMania.addPieceSpecial(board, 23))
  }

  test("testPlayGame 9 players 25 turns") {
    assert(32 === MarbleMania.playGame(9, 25))
  }

  test("testPlayGame 10 players 1618 turns") {
    assert(8317 === MarbleMania.playGame(10, 1618))
  }

  test("testPlayGame 13 players 7999 turns") {
    assert(146373 === MarbleMania.playGame(13, 7999))
  }

  test("testPlayGame 17 players 1104 turns") {
    assert(2764 === MarbleMania.playGame(17, 1104))
  }

  test("testPlayGame 21 players 6111 turns") {
    assert(54718 === MarbleMania.playGame(21, 6111))
  }

  test("testPlayGame 30 players 5807 turns") {
    assert(37305 === MarbleMania.playGame(30, 5807))
  }

  test("testPlayGame 491 players 71058 turns") {
    assert(361466 === MarbleMania.playGame(491, 71058))
  }

  test("testPlayGame 491 players 71058 * 100 turns") {
    assert(361466 === MarbleMania.playGame(491, 71058 * 100))
  }
}
