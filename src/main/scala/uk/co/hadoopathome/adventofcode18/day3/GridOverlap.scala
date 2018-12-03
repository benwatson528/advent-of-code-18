package uk.co.hadoopathome.adventofcode18.day3

object GridOverlap {
    def gridOverlap(ls: List[GridInstruction]): Int = {
        val gridCoords = ls.map(calculateGridCoords)
        val coordCounts = coordCombinerRec(gridCoords, Map[Coord, Int]())
        coordCounts.count(_._2 > 1)
    }

    def coordCombinerRec(ls: List[List[Coord]], countMap: Map[Coord, Int]): Map[Coord, Int] = {
        ls match {
            case x :: xs => coordCombinerRec(xs, addCoordsToMapRec(x, countMap))
            case _ => countMap
        }
    }

    def addCoordsToMapRec(ls: List[Coord], countMap: Map[Coord, Int]): Map[Coord, Int] = {
        ls match {
            case x :: xs => addCoordsToMapRec(xs, countMap + (x -> (countMap.getOrElse(x,0) + 1)))
            case _ => countMap
        }
    }

    def calculateGridCoords(gI: GridInstruction): List[Coord] = {
        val coords = scala.collection.mutable.ArrayBuffer.empty[Coord]
        for (i <- gI.left until gI.left + gI.width) {
            for (j <- gI.top until gI.top + gI.height) {
                coords += Coord(i, j)
            }
        }
        coords.toList
    }

    case class GridInstruction(boxId: Int, left: Int, top: Int, width: Int, height: Int)

    case class Coord(x: Int, y: Int)
}
