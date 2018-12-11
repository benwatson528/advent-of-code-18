package uk.co.hadoopathome.adventofcode18.day11

import org.scalatest.FunSuite

class ChronalChangeTest extends FunSuite {
    ignore("testFindMostPowerful 18") {
        assert("90,269,16" === ChronalChange.findMostPowerful(18))
    }

    ignore("testFindMostPowerful 42") {
        assert("232,251,12" === ChronalChange.findMostPowerful(42))
    }

    ignore("testFindMostPowerful real") {
        assert("236,146,12" === ChronalChange.findMostPowerful(8561))
    }
}
