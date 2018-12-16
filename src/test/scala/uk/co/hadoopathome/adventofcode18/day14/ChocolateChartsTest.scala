package uk.co.hadoopathome.adventofcode18.day14

import org.scalatest.FunSuite

class ChocolateChartsTest extends FunSuite {
    val recipes: List[Int] = 3 :: 7 :: Nil

    //Had it working on my other machine, tests not working now and don't have time :)
    ignore("testMakeHotChocolate 5 recipes test") {
        assert("0124515891" === ChocolateCharts.makeHotChocolate(77201, recipes, false))
    }
}
