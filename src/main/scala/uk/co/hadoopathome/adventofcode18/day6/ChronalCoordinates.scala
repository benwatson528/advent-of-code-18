package uk.co.hadoopathome.adventofcode18.day6

object ChronalCoordinates {

    def getLargestPointArea(points: List[(Int, Int)]): Int = {
        val (maxX, maxY) = findLimits(points)
        val closestPoints = for (i <- 0 to maxX; j <- 0 to maxY) yield findClosest(i, j, maxX, maxY, points)
        val groupedNotBorderPoints = closestPoints.filter(_._1 != -1).groupBy(_._1)
            .mapValues(_.map(_._2)).filterNot(_._2.contains(true))
        groupedNotBorderPoints.mapValues(_.size).maxBy(_._2)._2
    }

    def findSafeRegionSize(points: List[(Int, Int)], maxDistance: Int): Int = {
        val (maxX, maxY) = findLimits(points)
        val pointDistances = for (i <- 0 to maxX; j <- 0 to maxY) yield findRegionDistance(i, j, points)
        pointDistances.count(_ < maxDistance)
    }

    def findClosest(x: Int, y: Int, maxX: Int, maxY: Int, points: List[(Int, Int)]): (Int, Boolean) = {
        val indexAndDistance = points.map(p => manhattanDistance(p, (x, y))).zipWithIndex.sortBy(_._1)
        val minDistance = indexAndDistance.head._1
        val isBorder = x == 0 || y == 0 || x == maxX || y == maxY
        if (indexAndDistance.count(p => p._1 == minDistance) != 1)
            (-1, isBorder)
        else
            (indexAndDistance.head._2, isBorder)
    }

    def findRegionDistance(x: Int, y: Int, points: List[(Int, Int)]): Int =
        points.map(p => manhattanDistance(p, (x, y))).sum


    def findLimits(points: List[(Int, Int)]): (Int, Int) = (points.maxBy(_._1)._1, points.maxBy(_._2)._2)

    def manhattanDistance(a: (Int, Int), b: (Int, Int)): Int = Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)
}
