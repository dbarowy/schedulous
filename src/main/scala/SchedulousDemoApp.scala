import Constraints.{ConsAvgWorkload, ConsNoConcurrentSlots, _}
import Core._
import Readers._

object SchedulousDemoApp extends App {
  // paths
  val volPath = "/Users/dbarowy/OneDrive/UMass/Volunteer/PLDI 2016/assignment_data/volunteers.csv"
  val evtPath = "/Users/dbarowy/OneDrive/UMass/Volunteer/PLDI 2016/assignment_data/events.csv"

  // constraint config
  val MAXSLOTS  = 4
  val MINSLOTS  = 1
  val MAXDAYS   = 2
  val MINUTEEPS = 240

  // load data
  val people = VolunteerCSVReader(volPath).peopleWhoCanServe(false)
  val events = AssignmentCSVReader(evtPath).days

  // constraint config
  val conf = (peopleMap: People#PeopleMap, slotMap: Timeslots#SlotMap, oldSchedule: Option[Schedule]) => {
    val c1 = ConsFillSlots(peopleMap, slotMap, oldSchedule)
    val c2 = ConsMaxSlots(MAXSLOTS, peopleMap, slotMap)
    val c3 = ConsMinSlots(MINSLOTS, peopleMap, slotMap)
    val c4 = ConsMaxDays(MAXDAYS, events, peopleMap, slotMap)
    val c5 = ConsWorkload(peopleMap, slotMap, oldSchedule)
    val c6 = ConsAvgWorkload(MINUTEEPS, c5.name, peopleMap, slotMap)
    val c7 = ConsNoConcurrentSlots(peopleMap, slotMap)

    List(
      c1,
      c2,
      c3,
      c4,
//      c5,
//      c6,
      c7
    )
  }

  // find schedule
  val schedule = Schedule.find(conf, people, events)

  // print schedule
  schedule match {
    case Some(s) => println(s)
    case None => "Cannot find schedule that meets constraints."
  }
}