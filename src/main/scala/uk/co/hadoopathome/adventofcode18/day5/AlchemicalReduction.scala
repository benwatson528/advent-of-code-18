package uk.co.hadoopathome.adventofcode18.day5

object AlchemicalReduction {
    def reducePolymer(polymer: String): String = {
        val string = polymer.scanLeft("")((a, b) => reduceAdjacent(a, b)).last
        string
    }

    def reduceAdjacent(a: String, b: Char): String = {
        b match {
            case x if a.isEmpty => x.toString
            case x if isCapitalisedMatch(a.last, x) => a.dropRight(1)
            case x => a + x
        }
    }

    def isCapitalisedMatch(a: Char, b: Char): Boolean = {
        a != b && Character.toUpperCase(a) == Character.toUpperCase(b)
    }
}
