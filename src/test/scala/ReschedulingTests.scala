import java.time.{LocalDate, LocalTime, Month}

import Constraints.{ConsFairWorkload, ConsMaxDays, ConsNoConcurrentSlots, ConsWorkload, _}
import Core._
import org.scalatest.{FlatSpec, Matchers}

class ReschedulingTests extends FlatSpec with Matchers {
  "Approval" should "fix slots so that rescheduling does not change them" in {
    // schedule
    val Sunday = LocalDate.of(2016, Month.JUNE, 13)

    val SundayEvents = Day(
      Sunday,
      Set(
        Timeslot("WORKSHOP1", "WORKSHOP1", "lover", LocalTime.of(9,0), LocalTime.of(10,0)),
        Timeslot("WORKSHOP2", "WORKSHOP2", "hater", LocalTime.of(10,0), LocalTime.of(11,0)),
        Timeslot("WORKSHOP3", "WORKSHOP3", "lover", LocalTime.of(12,0), LocalTime.of(13,0)),
        Timeslot("WORKSHOP4", "WORKSHOP4", "hater", LocalTime.of(14,0), LocalTime.of(15,0))
      )
    )

    // days
    val events = Set(SundayEvents)

    // people
    val people = Set(
      Person("Person", "One", Availability.AllDay(Sunday)),
      Person("Person", "Two", Availability.AllDay(Sunday)),
      Person("Person", "Three", Availability.AllDay(Sunday)),
      Person("Person", "Four", Availability.AllDay(Sunday))
    )

    // constraint config
    val MAXSLOTS  = 2
    val MINSLOTS  = 1
    val MAXDAYS   = 1
    val MINUTEEPS = 90

    // constraint loader
    val conf = (peopleMap: People#PeopleMap, slotMap: Timeslots#SlotMap, oldSchedule: Option[Schedule]) => {
      val c1 = ConsFillSlots(peopleMap, slotMap, oldSchedule)
      val c2 = ConsMaxSlots(MAXSLOTS, peopleMap, slotMap)
      val c3 = ConsMinSlots(MINSLOTS, peopleMap, slotMap)
      val c4 = ConsMaxDays(MAXDAYS, events, peopleMap, slotMap)
      val c5 = ConsWorkload(peopleMap, slotMap)
      val c6 = ConsFairWorkload(MINUTEEPS, c5.name, peopleMap, slotMap)
      val c7 = ConsNoConcurrentSlots(peopleMap, slotMap)

      List(
        c1,
        c2,
        c3,
        c4,
        c5,
        c6,
        c7
      )
    }

    val schedule = Schedule.find(conf, people, events)

    // get schedule
    schedule match {
      case Some(s) =>
        println("Schedule #1:\n\n" + s)

        // choose an assignment to accept
        val approved = s.assignments(0).approve()

        // choose an assignment to reject
        val rejected = s.assignments(1).reject()

        s.update(conf, List(approved, rejected)) match {
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
          case None =>

            fail()
        }
      case None => fail()
    }
  }
}
