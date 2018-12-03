package uk.co.hadoopathome.adventofcode18.day3

object GridOwner {
    def findNumberOverlapping(ls: List[GridInstruction]): Int = {
        val coordCounts = getCoordCounts(ls)
        coordCounts.count(_._2.size > 1)
    }

    def findNotOverlapped(ls: List[GridInstruction]): Int = {
        val coordCounts = getCoordCounts(ls)
        ls.map(_.boxId).filter(isNotOverlapping(coordCounts, _)).head
    }

    private def getCoordCounts(ls: List[GridInstruction]): Map[Coord, Set[Int]] = {
        val gridCoords = ls.map(calculateGridCoords)
        coordCombinerRec(gridCoords, Map[Coord, Set[Int]]())
    }

    def isNotOverlapping(coordCounts: Map[Coord, Set[Int]], boxId: Int): Boolean = {
        !coordCounts.exists(x => x._2.size > 1 && x._2.contains(boxId))
    }

    def coordCombinerRec(ls: List[List[CoordAndBoxId]],
                         countMap: Map[Coord, Set[Int]]): Map[Coord, Set[Int]] = {
        ls match {
            case x :: xs => coordCombinerRec(xs, addCoordsToMapRec(x, countMap))
            case _ => countMap
        }
    }

    def addCoordsToMapRec(ls: List[CoordAndBoxId], countMap: Map[Coord, Set[Int]]): Map[Coord, Set[Int]] = {
        ls match {
            case x :: xs => addCoordsToMapRec(xs,
                countMap + (x.coord -> (countMap.getOrElse(x.coord, Set()) + x.boxId)))
            case _ => countMap
        }
    }

    def calculateGridCoords(gI: GridInstruction): List[CoordAndBoxId] = {
        val coords = scala.collection.mutable.ArrayBuffer.empty[CoordAndBoxId]
        for (i <- gI.left until gI.left + gI.width) {
            for (j <- gI.top until gI.top + gI.height) {
                coords += CoordAndBoxId(Coord(i, j), gI.boxId)
            }
        }
        coords.toList
    }
}
