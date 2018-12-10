package uk.co.hadoopathome.adventofcode18.day9

object MarbleMania {
    type Board = (Int, List[Int])

    def playGame(numPlayers: Int, numRounds: Int): Int = {
        val board = (0, 0 :: Nil)
        val players = List.fill(numPlayers)(0)
        val finalScores = takeTurn(board, players, numRounds, 1)
        println(finalScores)
        finalScores.max
    }

    def takeTurn(board: Board, players: List[Int], numRounds: Int, round: Int): List[Int] = {
        if (round <= numRounds) {
            if (round % 23 == 0) {
                val (updatedBoard: Board, score: Int) = addPieceSpecial(board, round)
                val updatedPlayers = updatePlayers(players, score, round)
                takeTurn(updatedBoard, updatedPlayers, numRounds, round + 1)
            } else {
                takeTurn(addPieceNormal(board, round), players, numRounds, round + 1)
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
        var position = 0
        if (board._1 - 7 < 0) {
            position = board._2.size - Math.abs(board._1 - 7)
        } else {
            position = board._1 - 7
        }

        if (position == -1) {
            ((0, board._2.take(board._2.size - 1)), board._2.head + 23)
        } else {
            ((position, board._2.take(position) ::: board._2.drop(position + 1)), board._2(position) + 23)
        }
    }

    def updatePlayers(players: List[Int], score: Int, round: Int): List[Int] = {
        val updatedPlayers = players.take((round - 1) % players.size) :::
            (players((round - 1) % players.size) + score) ::
            players.drop(round % players.size)
        updatedPlayers
    }
}
