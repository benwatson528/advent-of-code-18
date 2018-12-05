package uk.co.hadoopathome.adventofcode18.day5

object AlchemicalReduction {
    def reducePolymer(polymer: String): String = {
        polymer.scanLeft("")((a, b) => reduceAdjacent(a, b)).last
    }

    def biggestReduction(polymer: String): Int = {
        val reducedPolymers = for (i <- 'a' to 'z') yield reducePolymer(removeLetter(i, polymer))
        reducedPolymers.map(_.length).min
    }

    def reduceAdjacent(a: String, b: Char): String = {
        b match {
            case x if a.isEmpty => x.toString
            case x if isCapitalisedMatch(a.last, x) => a.dropRight(1)
            case x => a + x
        }
    }

    def removeLetter(i: Char, polymer: String): String = {
        polymer.filter(x => Character.toLowerCase(x) != i)
    }

    def isCapitalisedMatch(a: Char, b: Char): Boolean = {
        a != b && Character.toUpperCase(a) == Character.toUpperCase(b)
    }
}
