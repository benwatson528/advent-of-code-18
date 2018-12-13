package uk.co.hadoopathome.adventofcode18

package object day13 {
    type Coord = (Int, Int)
    type Track = (Coord, Char)

    sealed trait Direction { def movement: Coord }
    case object LEFT extends Direction { val movement = (-1, 0) }
    case object UP extends Direction { val movement = (0, -1) }
    case object DOWN extends Direction { val movement = (0, 1) }
    case object RIGHT extends Direction { val movement = (1, 0) }

    sealed trait Turn { def nextTurn: Turn }
    case object TURN_LEFT extends Turn { val nextTurn  = TURN_STRAIGHT}
    case object TURN_STRAIGHT extends Turn { val nextTurn  = TURN_RIGHT}
    case object TURN_RIGHT extends Turn { val nextTurn  = TURN_LEFT}

    case class Cart(position: Coord, direction: Direction, turn: Turn)
}
