package uk.co.hadoopathome.adventofcode18

package object day15 {
    case class Coord(x: Int, y: Int)
    case class Elf(coord: Coord, attackPower: Int, hp: Int)
    case class Goblin(coord: Coord, attackPower: Int, hp: Int)
}
