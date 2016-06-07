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

        s.update(List(approved, rejected)) match {
          case Some(s2) => {
            println("Schedule #2:\n\n" + s2)

            val accAndProp = s2.assignments.foldLeft(Map[String,List[Assignment]]()) {
              case (acc, a) =>
                if (acc.contains(a.slotname.name)) {
                  acc + (a.slotname.name -> (a :: acc(a.slotname.name)))
                } else {
                  acc + (a.slotname.name -> List(a))
                }
            }

            // the final schedule should contain the approved slot with the same person
            accAndProp(approved.slotname.name)
              .exists(a =>
                a.person == approved.person &&
                  a.approval == Approved
              ) should be (true)

            // the final schedule should contain the rejected slot with the same person
            accAndProp(rejected.slotname.name)
              .exists(a =>
                a.person == rejected.person &&
                  a.approval == Rejected
              ) should be (true)

            // the final schedule should contain the rejected slot with an alternate proposed assignment
            accAndProp(rejected.slotname.name)
              .exists(a =>
                a.person != rejected.person &&
                  a.approval == Proposed
              ) should be (true)

            // everything that wasn't explicitly accepted or rejected in the
            // last step must be proposed
            s2.assignments
              .filter { a => a.id != approved.id && a.id != rejected.id }
              .forall { a => a.approval == Proposed } should be (true)
          }
          case None => fail()
        }
      case None => fail()
    }
  }
}
