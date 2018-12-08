package uk.co.hadoopathome.adventofcode18.day7

object PathFinder {
  type Steps = Map[Char, List[Char]]

  def findPath(input: List[(Char, Char)], workers: WorkerManager): (String, Int) = {
    val totalSteps = input.flatMap(x => x._1 :: x._2 :: Nil).toSet
    val groupedSteps = input.groupBy(_._1).mapValues(_.map(_._2))
    val startingSteps = totalSteps.diff(groupedSteps.keySet).map(x => x -> List()).toMap
    val (pathTraversed, totalTime) = traverse(groupedSteps ++ startingSteps, List(), workers)
    (pathTraversed.mkString, totalTime)
  }

  def traverse(steps: Steps, traversed: List[Char], workers: WorkerManager): (List[Char], Int) = {
    val updatedSteps = updateSteps(workers, steps)
    val finishedTraversal = if (workers.getFinished().nonEmpty) traversed ::: workers.getFinished() else traversed
    workers.updateFinished()
    if (updatedSteps.isEmpty && workers.areAllFinished()) {
      (finishedTraversal, workers.totalTime)
    } else if (workers.canConsume(updatedSteps)) {
      val nextStep = updatedSteps.toList.minBy(s => (s._2.size, s._1))._1
      workers.assign(nextStep)
      traverse(updatedSteps.filter(s => s._1 != nextStep), finishedTraversal, workers)
    } else {
      workers.tick()
      traverse(updatedSteps, finishedTraversal, workers)
    }
  }

  def updateSteps(workers: WorkerManager, steps: Steps): Steps = updateStepsRec(workers.workers, steps: Steps): Steps

  def updateStepsRec(ls: List[(Char, Int)], steps: Steps): Steps = {
    ls match {
      case x :: xs if x._2 == 0 && x._1 != ' ' => updateStepsRec(xs, removeTraversedStep(steps, x._1))
      case _ :: xs => updateStepsRec(xs, steps)
      case _ => steps
    }
  }

  def removeTraversedStep(steps: Steps, traversedStep: Char): Steps =
    steps.mapValues(_.filter(_ != traversedStep)).filter(_._1 != traversedStep)
}
