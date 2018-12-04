package uk.co.hadoopathome.adventofcode18.day4

object SleepMonitor {
    def findSleepiestGuard(ls: List[ActivityLog]): ID = {
        val sleepMap = findSleepiestGuardRec(ls, ls.head.guardId, -1, Map())
        sleepMap.maxBy(_._2)._1
    }

    def findMinuteMostAsleep(ls: List[ActivityLog], guardId: ID): Int = {
        val guardSleepLogs = ls.filter(x => x.guardId == guardId && x.status != STARTS_SHIFT)
        val pairedTimes = guardSleepLogs.sliding(2, 2).map(x => (x.head, x(1))).toList
        val allMinutes = pairedTimes.flatMap(x => for(i <-x._1.minute until x._2.minute) yield i)
        allMinutes.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
    }

    def findSleepiestGuardRec(ls: List[ActivityLog], guardId: ID, sleepStart: Int,
                              countMap: Map[ID, Int]): Map[ID, Int] = {
        ls match {
            case x :: xs if x.status == STARTS_SHIFT =>
                findSleepiestGuardRec(xs, x.guardId, -1, countMap)
            case x :: xs if x.status == FALLS_ASLEEP =>
                findSleepiestGuardRec(xs, guardId, x.minute, countMap)
            case x :: xs if x.status == WAKES_UP =>
                findSleepiestGuardRec(xs, guardId, -1, countMap +
                    (guardId -> (countMap.getOrElse(guardId, 0) + (x.minute-sleepStart))))
            case _ => countMap
        }
    }
    }
