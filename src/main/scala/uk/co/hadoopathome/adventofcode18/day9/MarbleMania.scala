package uk.co.hadoopathome.adventofcode18.day9

import scala.collection.mutable.ArrayBuffer

object MarbleMania {
    type Board = (Int, ArrayBuffer[Long])

    def playGame(numPlayers: Int, numRounds: Int): Long = {
        val board = (0, ArrayBuffer(0l))
        val players = ArrayBuffer.fill(numPlayers)(0l)
        val finalScores = takeTurn(board, players, numRounds, 1, 0)
        finalScores.max
    }

    def takeTurn(board: Board, players: ArrayBuffer[Long], numRounds: Int, round: Int, playerNum: Int): ArrayBuffer[Long] = {
        if (round < numRounds) {
            if (round % 23 == 0) {
                val (updatedBoard, score) = addPieceSpecial(board, round)
                players.update(playerNum, players(playerNum) + score)
               // val updatedPlayers = updateList(players, playerNum, score)
                takeTurn(updatedBoard, players, numRounds, round + 1, (playerNum + 1) % players.size)
            } else {
                takeTurn(addPieceNormal(board, round), players, numRounds, round + 1, (playerNum + 1) % players.size)
            }
        } else {
            players
        }
    }

    def addPieceNormal(board: Board, round: Int): Board = {
        if (board._1 + 2 == board._2.size) {
            (board._2.size, board._2 :+ round.toLong)
        } else {
            val position = (board._1 + 2) % board._2.size
            board._2.insert(position, round)
            (position, board._2)
        }
    }

    def addPieceSpecial(board: Board, round: Int): (Board, Long) = {
        val boardSize = board._2.size
        var position = 0
        if ((board._1 - 7) < 0) {
            position = boardSize - (Math.abs(board._1 - 7) % boardSize)
        } else {
            position = board._1 - 7
        }
        val newPosition = if (position == boardSize - 1) 0 else position
        val score = board._2(position) + round
        board._2.remove(position)
        ((newPosition, board._2), score)
    }
}
