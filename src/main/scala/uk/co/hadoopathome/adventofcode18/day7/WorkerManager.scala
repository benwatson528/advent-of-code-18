package uk.co.hadoopathome.adventofcode18.day7

import uk.co.hadoopathome.adventofcode18.day7.PathFinder.Steps

class WorkerManager(var workers: List[(Char, Int)], tickBase: Int, var totalTime: Int = 0) {
    def canConsume(steps: Steps): Boolean = steps.exists(s => s._2.isEmpty) && isWorkerAvailable

    def areAllFinished(): Boolean = workers.count(w => w._1 == '.' && w._2 == 0) == workers.size

    def getFinished(): List[Char] = workers.filter(w => w._1 != '.' && w._2 == 0).map(_._1)

    def updateFinished() = workers = workers.map(w => if (w._2 == 0) ('.', 0) else w)

    def isWorkerAvailable: Boolean = workers.count(_._2 == 0) > 0

    def tick(): Unit = {
        workers = workers.map(w => if (w._2 > 0) (w._1, w._2 - 1) else w)
        totalTime = totalTime + 1
    }

    def assign(step: Char): Unit = {
        val sorted = workers.sortBy(w => w._2)
        workers = (step, tickBase + charToTime(step)) :: sorted.tail
    }

    def charToTime(step: Char): Int = step.toInt - 64
}
