package uk.co.hadoopathome.adventofcode18.day7

object PathFinder {
  type Steps = Map[Char, List[Char]]

  def findPath(input: List[(Char, Char)]): String = {
    val totalSteps = input.flatMap(x => x._1 :: x._2 :: Nil).toSet
    val groupedSteps = input.groupBy(_._1).mapValues(_.map(_._2))
    val startingSteps = totalSteps.diff(groupedSteps.keySet).map(x => x -> List()).toMap
    traverse(groupedSteps ++ startingSteps, List()).mkString
  }

  def traverse(steps: Steps, traversed: List[Char]): List[Char] = {
    if (steps.nonEmpty) {
      val toTraverse =  steps.toList.minBy(s => (s._2.size, s._1))._1
      traverse(removeTraversedStep(steps, toTraverse), traversed :+ toTraverse)
    } else {
      traversed
    }
  }

  def removeTraversedStep(groupedSteps: Steps, traversedStep: Char): Steps = {
    val removedValues = groupedSteps.mapValues(_.filter(_ != traversedStep))
    val removedKey = removedValues.filter(_._1 != traversedStep)
    removedKey
  }
}
