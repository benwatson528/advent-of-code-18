package uk.co.hadoopathome.adventofcode18.day4

import org.scalatest.FunSuite

import scala.io.Source

class SleepMonitorTest extends FunSuite {
    test("findSleepiestGuard test input") {
        val incompleteInput = Source.fromResource("day4/test-input.txt").getLines.toList
            .map(convertToActivityLog).sortBy(x => (x.month, x.day,  x.hour, x.minute))
        val input = populateGuardIdsRec(incompleteInput, incompleteInput.head.guardId, List())
        assert(10 === SleepMonitor.findSleepiestGuard(input))
    }

    test("findMinuteMostAsleep test input") {
        val incompleteInput = Source.fromResource("day4/test-input.txt").getLines.toList
            .map(convertToActivityLog).sortBy(x => (x.month, x.day,  x.hour, x.minute))
        val input = populateGuardIdsRec(incompleteInput, incompleteInput.head.guardId, List())
        val sleepiestGuard = SleepMonitor.findSleepiestGuard(input)
        val (minuteMostAsleep, _) = SleepMonitor.findMinuteMostAsleep(input, sleepiestGuard)
        assert(10 === sleepiestGuard)
        assert(24 === minuteMostAsleep)
        assert(240 === minuteMostAsleep * sleepiestGuard)
    }

    test("findMinuteMostAsleep input") {
        val incompleteInput = Source.fromResource("day4/input.txt").getLines.toList
            .map(convertToActivityLog).sortBy(x => (x.month, x.day,  x.hour, x.minute))
        val input = populateGuardIdsRec(incompleteInput, incompleteInput.head.guardId, List())
        val sleepiestGuard = SleepMonitor.findSleepiestGuard(input)
        val (minuteMostAsleep, _) = SleepMonitor.findMinuteMostAsleep(input, sleepiestGuard)
        assert(2039 === sleepiestGuard)
        assert(49 === minuteMostAsleep)
        assert(99911 === minuteMostAsleep * sleepiestGuard)
    }

    test("findMinuteMostAsleepForAll test input") {
        val incompleteInput = Source.fromResource("day4/test-input.txt").getLines.toList
            .map(convertToActivityLog).sortBy(x => (x.month, x.day,  x.hour, x.minute))
        val input = populateGuardIdsRec(incompleteInput, incompleteInput.head.guardId, List())
        val (guardId, minuteMostAsleep) = SleepMonitor.findMinuteMostAsleepForAll(input)
        assert(99 === guardId)
        assert(45 === minuteMostAsleep)
        assert(4455 === minuteMostAsleep * guardId)
    }

    test("findMinuteMostAsleepForAll input") {
        val incompleteInput = Source.fromResource("day4/input.txt").getLines.toList
            .map(convertToActivityLog).sortBy(x => (x.month, x.day,  x.hour, x.minute))
        val input = populateGuardIdsRec(incompleteInput, incompleteInput.head.guardId, List())
        val (guardId, minuteMostAsleep) = SleepMonitor.findMinuteMostAsleepForAll(input)
        assert(1733 === guardId)
        assert(38 === minuteMostAsleep)
        assert(65854 === minuteMostAsleep * guardId)
    }

    def convertToActivityLog(s: String): ActivityLog = {
        val dateStatusSplit = s.split("] ")
        val date = dateStatusSplit(0).split(' ')(0)
        val month = date.split('-')(1).toInt
        val day = date.split('-')(2).toInt
        val time = s.split(']')(0).split(' ')(1)
        val hour = time.split(':')(0).toInt
        val minute = time.split(':')(1).toInt
        val status = convertToStatus(dateStatusSplit(1))
        var guardId = -1
        if (status == STARTS_SHIFT) {
            val pattern = "\\d+".r
            guardId = pattern.findFirstIn(dateStatusSplit(1)).get.toInt
        }
        ActivityLog(month, day, hour, minute, guardId, status)
    }

    def convertToStatus(s: String): GuardStatus = {
        s match {
            case x if x.startsWith("Guard") => STARTS_SHIFT
            case x if x.startsWith("wakes") => WAKES_UP
            case x if x.startsWith("falls") => FALLS_ASLEEP
        }
    }

    def populateGuardIdsRec(ls: List[ActivityLog], guardId: ID,
                            outList: List[ActivityLog]): List[ActivityLog] = {
        ls match {
            case x :: xs if x.status == STARTS_SHIFT => populateGuardIdsRec(xs, x.guardId, outList :+ x)
            case x :: xs => populateGuardIdsRec(xs, guardId, outList :+
                ActivityLog(x.month, x.day, x.hour, x.minute, guardId, x.status))
            case _ => outList
        }
    }
}
