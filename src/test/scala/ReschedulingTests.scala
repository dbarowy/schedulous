import java.time.{LocalDate, LocalTime, Month}

import Core._
import org.scalatest.{FlatSpec, Matchers}

class ReschedulingTests extends FlatSpec with Matchers {
  "Approval" should "fix slots so that rescheduling does not change them" in {
    // schedule
    val Sunday = LocalDate.of(2016, Month.JUNE, 13)

    val SundayEvents = Day(
      Sunday,
      Set(
        Timeslot("WORKSHOP1", LocalTime.of(9,0), LocalTime.of(10,0)),
        Timeslot("WORKSHOP2", LocalTime.of(10,0), LocalTime.of(11,0)),
        Timeslot("WORKSHOP3", LocalTime.of(12,0), LocalTime.of(13,0)),
        Timeslot("WORKSHOP4", LocalTime.of(14,0), LocalTime.of(15,0))
      )
    )

    // people
    val people = Set(
      Person("Person", "One", Availability.AllDay(Sunday)),
      Person("Person", "Two", Availability.AllDay(Sunday)),
      Person("Person", "Three", Availability.AllDay(Sunday)),
      Person("Person", "Four", Availability.AllDay(Sunday))
    )

    // days
    val events = Set(SundayEvents)

    val schedule = Schedule.find(people, events)

    // get schedule
    schedule match {
      case Some(s) =>
        println("Schedule #1:\n\n" + s)

        // choose an assignment to accept
        val approved = s.assignments(0).approve()

        // choose an assignment to reject
        val rejected = s.assignments(1).reject()

        val changes = approved :: rejected :: s.assignments.slice(2,s.assignments.length).toList

        s.update(changes) match {
          case Some(s2) => {
            println("Schedule #2:\n\n" + s2)

            val s2Map = s2.assignments.map { a => a.id -> a }.toMap
            s2Map(approved.id).person == approved.person should be (true)
            s2Map(approved.id).approval == approved.approval should be (true)
            s2Map(rejected.id).person != rejected.person should be (true)
            s2Map(rejected.id).approval != rejected.approval should be (true)
            s2.assignments
              .filter { a => a.id != approved.id && a.id != rejected.id }
              .forall { a => a.approval == Unapproved } should be (true)
          }
          case None => fail()
        }
      case None => fail()
    }
  }
}
