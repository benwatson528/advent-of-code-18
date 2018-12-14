package uk.co.hadoopathome.adventofcode18.day14

object ChocolateCharts {
    def makeHotChocolate(input: Int, recipes: List[Int], reverse: Boolean): String = {
        if (!reverse) {
            makeHotChocolateRec(recipes, (0, 1), input + 10).mkString
        }
        else {
            makeHotChocolateReverseRec(recipes.toVector, (0, 1), input.toString).toString
        }
    }

    def makeHotChocolateRec(recipes: List[Int], workerPositions: (Int, Int), recipeSize: Int): List[Int] = {
        val (score1, score2) = (recipes(workerPositions._1), recipes(workerPositions._2))
        val newScore = score1 + score2
        val separated = newScore.toString.map(_.toInt - 48).toList
        val updatedRecipes = recipes ::: separated

        if (updatedRecipes.size == recipeSize) {
            updatedRecipes.takeRight(10)
        } else {
            makeHotChocolateRec(updatedRecipes,
                ((workerPositions._1 + 1 + score1) % updatedRecipes.size,
                    (workerPositions._2 + 1 + score2) % updatedRecipes.size), recipeSize)
        }
    }

    def makeHotChocolateReverseRec(recipes: Vector[Int], workerPositions: (Int, Int), pattern: String): Int = {
        if (recipes.size % 1000000 == 0) println(recipes.size)
        val (score1, score2) = (recipes(workerPositions._1), recipes(workerPositions._2))
        val newScore = score1 + score2

        var updatedRecipes = Vector[Int]()
        if (newScore > 9) {
            val digits = newScore % 10
            val tens = (newScore - digits) / 10
            updatedRecipes = recipes :+ tens
            if (checkEnding(updatedRecipes, pattern)) {
                return updatedRecipes.mkString.indexOf(pattern)
            }
            updatedRecipes = updatedRecipes :+ digits
            if (checkEnding(updatedRecipes, pattern)) {
                return updatedRecipes.mkString.indexOf(pattern)
            }
        } else {
            updatedRecipes = recipes :+ newScore
            if (checkEnding(updatedRecipes, pattern)) {
                return updatedRecipes.mkString.indexOf(pattern)
            }
        }

        makeHotChocolateReverseRec(updatedRecipes,
            ((workerPositions._1 + 1 + score1) % updatedRecipes.size,
                (workerPositions._2 + 1 + score2) % updatedRecipes.size), pattern)
    }

    def checkEnding(recipes: Vector[Int], pattern: String): Boolean = {
        if (recipes.length < pattern.length + 1) return false
        if (recipes.last != (pattern.last.toInt - 48)) return false
        val endOfRecipes = recipes.takeRight(pattern.length)
        endOfRecipes.mkString == pattern
    }
}
