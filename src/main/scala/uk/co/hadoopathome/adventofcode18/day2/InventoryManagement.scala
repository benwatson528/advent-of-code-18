package uk.co.hadoopathome.adventofcode18.day2

object InventoryManagement {
    def generateChecksum(ls: List[List[Char]]): Int = {
        val numTwos = ls.map(x => hasOccurrencesInString(x, 2))
            .foldLeft(0)((acc, x) => if (x) acc + 1 else acc + 0)
        val numThrees = ls.map(x => hasOccurrencesInString(x, 3))
            .foldLeft(0)((acc, x) => if (x) acc + 1 else acc + 0)
        numTwos * numThrees
    }

    def hasOccurrencesInString(ls: List[Char], count: Int): Boolean = {
        val countMap = letterCounterRec(ls, Map[Char, Int]())
        containsValue(countMap, count)
    }

    private def containsValue(countMap: Map[Char, Int], value: Int) =
        countMap.count(p => p._2 == value) != 0


    def letterCounterRec(ls: List[Char], countMap: Map[Char, Int]): Map[Char, Int] = {
        ls match {
            case x :: xx => letterCounterRec(xx, countMap + (x -> (countMap.getOrElse(x,0) + 1)))
            case _ => countMap
        }
    }
}
