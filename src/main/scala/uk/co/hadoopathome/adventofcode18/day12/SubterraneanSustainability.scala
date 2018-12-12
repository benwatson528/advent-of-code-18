package uk.co.hadoopathome.adventofcode18.day12

object SubterraneanSustainability {
    type Pots = List[(Char, Int)]
    type Rules = Set[String]

    def processRecords(initialPots: Pots, rules: Rules, numIters: Int): Pots =
        processRecordsRec(initialPots: Pots, rules: Rules, numIters, 0)

    def processRecordsRec(pots: Pots, rules: Rules, numIters: Int, iter: Int): Pots = {
        if (iter > 9000) println(pots)
        if (iter == numIters) {
            pots
        } else {
            val paddedPots = padPots(pots)
            val newPots = paddedPots.sliding(5).map(processWindow(_, rules)).toList
            processRecordsRec(trimPots(newPots), rules, numIters, iter + 1)
        }
    }

    def padPots(pots: Pots): Pots = {
        val leftIdx = pots.head._2
        val rightIdx = pots.last._2
        (('.', leftIdx-4) :: ('.', leftIdx-3) :: ('.', leftIdx-2) ::
            ('.', leftIdx-1) :: Nil) ::: pots ::: (('.', rightIdx+1) ::
            ('.', rightIdx+2) :: ('.', rightIdx+3) :: ('.', rightIdx+4) :: Nil)
    }

    def trimPots(newPots: Pots): Pots = newPots.dropWhile(_._1 == '.').reverse.dropWhile(_._1 == '.').reverse


    def processWindow(window: Pots, rules: Rules): (Char, Int) = {
        val position = window(2)._2
        val line = window.map(w => w._1).mkString
        val newPotStatus = if (rules.contains(line)) '#' else '.'
        (newPotStatus, position)
    }
}
