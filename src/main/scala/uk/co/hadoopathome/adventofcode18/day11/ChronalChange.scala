package uk.co.hadoopathome.adventofcode18.day11

import scala.collection.mutable.ArrayBuffer

object ChronalChange {
    type Grid = ArrayBuffer[ArrayBuffer[Int]]

//    val grid = buildScoreGrid(8561)
//
//    private val mostPowerful = findMostPowerful(grid)
//    println("maxScoreCoords = " + mostPowerful._1._1 + "," + mostPowerful._1._2 + "," + mostPowerful._2)

    def buildScoreGrid(serialNo: Int): Grid = {
        val grid = scala.collection.mutable.ArrayBuffer.fill(301)(scala.collection.mutable.ArrayBuffer.fill(301)(0))
        for (y <- 1 to 300;
             x <- 1 to 300) {
            grid(x)(y) = {
                val rackId = x + 10
                val powerLevel = (((rackId * y) + serialNo) * rackId).toString
                if (powerLevel.length < 3) - 5 else powerLevel.charAt(powerLevel.length - 3).asDigit - 5
            }
        }
        grid
    }

    def findMostPowerful(serialNo: Int): String = {
        val grid = buildScoreGrid(serialNo)
        var maxScore = 0
        var maxScoreCoords = (0, 0)
        var maxSquareSize = 0
        for (y <- 1 to 300;
             x <- 1 to 300) {
            var currentScore = 0
            for (squareSize <- 1 to Math.min(300 - x, 300 - y)) {
                currentScore = getSquareScore(grid, x, y, squareSize)
                if (currentScore > maxScore) {
                    maxScore = currentScore
                    maxScoreCoords = (x, y)
                    maxSquareSize = squareSize
                }
            }
        }
        maxScoreCoords._1 + "," + maxScoreCoords._2 + "," + maxSquareSize
    }

    def getSquareScore(grid: Grid, topLeftX: Int, topLeftY: Int, squareSize: Int): Int = {
        var currentScore = 0
        for (miniY <- 0 until squareSize;
             miniX <- 0 until squareSize) {
            currentScore = currentScore + grid(topLeftX + miniX)(topLeftY + miniY)
        }
        currentScore
    }
}
