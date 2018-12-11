package uk.co.hadoopathome.adventofcode18.day10

object StarsAlign {
    type Position = (Int, Int)
    type Velocity = (Int, Int)
    type Space = List[(Position, Velocity)]

    def moveParticles(ls: Space, numIters: Int): Space = {
        moveParticlesRec(ls, 1, numIters, Integer.MAX_VALUE)
    }

    def moveParticlesRec(ls: Space, iter: Int, numIters: Int, sumBounds: Int): Space = {
        if (iter == numIters) {
            ls
        } else {
            val updatedMap = ls.map(point => ((point._1._1 + point._2._1, point._1._2 + point._2._2), point._2))
            val newBounds = draw(updatedMap, iter, sumBounds)
            moveParticlesRec(updatedMap, iter + 1, numIters, newBounds)
        }
    }

    def draw(ls: Space, iter: Int, sumBounds: Int): Int = {
        val positions = ls.map(_._1)
        val (xBounds, yBounds) = findMaxBounds(ls)
        val boundsSize = Math.abs(xBounds._1) + Math.abs(xBounds._2) + Math.abs(yBounds._1) + Math.abs(yBounds._2)
        if (iter == 10710) {
            for (j <- yBounds._1 to yBounds._2) {
                for (i <- xBounds._1 to xBounds._2) {
                    val char = if (positions.contains((i, j))) '#' else '.'
                    print(char)
                }
                println()
            }
            println()
        }
        boundsSize
    }

    def findMaxBounds(ls: Space): ((Int, Int), (Int, Int)) = {
        val xBounds = (ls.map(x => x._1._1).min, ls.map(x => x._1._1).max)
        val yBounds = (ls.map(x => x._1._2).min, ls.map(x => x._1._2).max)
        (xBounds, yBounds)
    }

}
