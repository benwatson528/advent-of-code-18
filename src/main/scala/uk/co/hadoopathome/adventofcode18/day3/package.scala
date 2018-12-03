package uk.co.hadoopathome.adventofcode18

package object day3 {
    case class GridInstruction(boxId: Int, left: Int, top: Int, width: Int, height: Int)
    case class Coord(x: Int, y: Int)
    case class CoordAndBoxId(coord: Coord, boxId: Int)
}
