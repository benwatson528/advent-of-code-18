package uk.co.hadoopathome.adventofcode18.day1

object ChronalCalibration {
    def sumNumbers(ls: List[Int]): Int = {
        sumNumbersRec(ls, 0)
    }

    def sumNumbersRec(ls: List[Int], sum: Int): Int = {
        ls match {
            case x :: xx => sumNumbersRec(xx, sum + x)
            case x :: Nil => sum + x
            case _ => sum
        }
    }
}
