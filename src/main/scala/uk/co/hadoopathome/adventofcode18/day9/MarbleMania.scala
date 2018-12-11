package uk.co.hadoopathome.adventofcode18.day9

object MarbleMania {
    type Board = (Int, List[Int])

    def playGame(numPlayers: Int, numRounds: Int): Int = {
        val board = (0, 0 :: Nil)
        val players = List.fill(numPlayers)(0)
        val finalScores = takeTurn(board, players, numRounds, 1, 0)
        println(finalScores)
        finalScores.max
    }

    def takeTurn(board: Board, players: List[Int], numRounds: Int, round: Int, playerNum: Int): List[Int] = {
        if (round < numRounds) {
            if (round % 23 == 0) {
                val (updatedBoard: Board, score: Int) = addPieceSpecial(board, round)
                val updatedPlayers = updateList(players, playerNum, score)
                takeTurn(updatedBoard, updatedPlayers, numRounds, round + 1, (playerNum + 1) % players.size)
            } else {
                takeTurn(addPieceNormal(board, round), players, numRounds, round + 1, (playerNum + 1) % players.size)
            }
        } else {
            players
        }
    }

    def addPieceNormal(board: Board, round: Int): Board = {
        if (board._1 + 2 == board._2.size) {
            (board._2.size, board._2 :+ round)
        } else {
            val position = (board._1 + 2) % board._2.size
            (position, board._2.take(position) ::: round :: board._2.drop(position))
        }
    }

    def addPieceSpecial(board: (Int, List[Int]), round: Int): (Board, Int) = {
        val boardSize = board._2.size
        var position = 0
        if ((board._1 - 7) < 0) {
            position = boardSize - (Math.abs(board._1 - 7) % boardSize)
        } else {
            position = board._1 - 7
        }
        val newPosition = if (position == boardSize - 1) 0 else position
        ((newPosition, removeFromList(board._2, position)), board._2(position) + 23)
    }

    def updateList(ls: List[Int], i: Int, value: Int): List[Int] = ls.zipWithIndex.map(x =>
        if (x._2 == i) x._1 + value else x._1)

    def removeFromList(ls: List[Int], i: Int): List[Int] = ls.zipWithIndex.filter(_._2 != i).map(_._1)
}
