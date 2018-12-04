package uk.co.hadoopathome.adventofcode18

package object day4 {
    type ID = Int

    case class ActivityLog(month: Int, day: Int, hour: Int, minute: Int, guardId: ID, status: GuardStatus)

    sealed trait GuardStatus
    case object STARTS_SHIFT extends GuardStatus
    case object WAKES_UP extends GuardStatus
    case object FALLS_ASLEEP extends GuardStatus
}
