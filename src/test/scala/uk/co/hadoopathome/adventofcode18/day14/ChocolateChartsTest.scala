package uk.co.hadoopathome.adventofcode18.day14

import org.scalatest.FunSuite

class ChocolateChartsTest extends FunSuite {
    val recipes: List[Int] = 3 :: 7 :: Nil

    ignore("testMakeHotChocolate 5 recipes test") {
        assert("0124515891" === ChocolateCharts.makeHotChocolate(5, recipes, false))
    }
}
