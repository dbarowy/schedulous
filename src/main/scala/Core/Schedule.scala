package Core

import Constraints._
import smtlib.parser.Commands.{DefineFun, FunDef}
import smtlib.parser.Terms.{SExpr, SNumeral, SSymbol}

object Schedule {
  def find(people: Set[Person], events: Set[Day], oldSchedule: Option[Schedule] = None) : Option[Schedule] = {
    // constants
    val MAXSLOTS  = 2
    val MINSLOTS  = 1
    val MAXDAYS   = 1
    val MINUTEEPS = 90

    // constraints
    val schedule = oldSchedule match {
      case None => Timeslots(events)
      case Some(s) => Timeslots(events, s.assignments.map { a => a.slotname -> a.slot }.toMap)
    }
    val participants = People(people)
    val c1 = ConsFillSlots(participants.peopleMap, schedule.slotMap, oldSchedule)
    val c2 = ConsMaxSlots(MAXSLOTS, participants.peopleMap, schedule.slotMap)
    val c3 = ConsMinSlots(MINSLOTS, participants.peopleMap, schedule.slotMap)
    val c4 = ConsMaxDays(MAXDAYS, events, participants.peopleMap, schedule.slotMap)
    val c5 = ConsWorkload(participants.peopleMap, schedule.slotMap, oldSchedule)
    val c6 = ConsAvgWorkload(MINUTEEPS, c5.name, participants.peopleMap, schedule.slotMap)
    val c7 = ConsNoConcurrentSlots(participants.peopleMap, schedule.slotMap)

    // solver
    val solver = Solver(Seq(schedule,participants,c1,c2,c3,c4,c5,c6,c7), debug = true)

    solver.model match {
      case Some(mod) => Some(parseModel(mod, schedule.slotMap, participants.peopleMap, oldSchedule))
      case None => None
    }
  }

  def parseModel(model: List[SExpr], slotmap: Timeslots#SlotMap, peoplemap: People#PeopleMap, oldSchedule: Option[Schedule]) : Schedule = {
    val rx = """\(define-fun (.+) \(\) Int ([0-9]+)\)""".r

    val assignments: List[Assignment] = model.flatMap { expr =>
      expr match {
        case DefineFun(fd: FunDef) =>
          val slotname = fd.name

          if (slotmap.contains(slotname)) {
            fd.body match {
              case SNumeral(i) =>
                val personint = i.toInt
                val slot = slotmap(slotname)
                val person = peoplemap(personint)

                Some(Assignment(slotname, slot,person,Unapproved))
              case _ => None
            }
          } else {
            None
          }
        case _ => None
      }
    }

    val fixed: List[Assignment] = oldSchedule match {
      case Some(schedule) => schedule.assignments.flatMap { assn =>
        if (assn.approval == Rejected || assn.approval == Approved) {
          Some(assn)
        } else {
          None
        }
      }.toList
      case None => List.empty
    }

    Schedule((assignments ::: fixed).sortWith { case (a1, a2) => a1.slot.start.isBefore(a2.slot.start)})
  }
}

case class Schedule(assignments: Seq[Assignment]) {

  val dsLookup = assignments.map { a => a.slot -> a }.toMap
  val pLookup : Map[Person,List[Assignment]] =
    assignments.foldLeft(assignments.map { a => a.person -> List[Assignment]() }.toMap){
      case (m,a) =>
        m + (a.person -> (a :: m(a.person)))
    }

  override def toString: String = {
    println("start,end,eventname,person,approval")
    assignments
      .sortWith { case (a1,a2) => a1.slot.start.isBefore(a2.slot.start) }
      .map { a =>
        "\"" + a.slot.start + "\",\"" + a.slot.end + "\",\"" + a.slot.eventname + "\",\"" + a.person.fname + " " + a.person.lname + "\",\"" + a.approval + "\""
      }.mkString("\n")
  }

  def people : Set[Person] = assignments.map { assn => assn.person }.toSet
  def days : Set[Day] = {
    assignments.map { assn => assn.slot }.groupBy{ ds => ds.start.toLocalDate }.map { case (date,dslots) =>
        Day(date, dslots.map { ds => Timeslot(ds.eventname, ds.start.toLocalTime, ds.end.toLocalTime) }.toSet)
    }.toSet
  }

  def update(changes: Seq[Assignment]) : Option[Schedule] = {
    assert(changes.size <= assignments.size)

    // update assignments with set of changes
    val cMap = changes.map { a => a.id -> a }.toMap
    val assignments2 = assignments.map { a =>
      if (cMap.contains(a.id)) {
        cMap(a.id)
      } else {
        a
      }
    }

    Schedule.find(people, days, Some(Schedule(assignments2)))
  }

  def getAssignmentFor(slot: Dateslot) : Option[Assignment] = {
    if (dsLookup.contains(slot)) {
      Some(dsLookup(slot))
    } else {
      None
    }
  }

  def workloadFor(p: Person, approval: Approval) : Double =
    if (pLookup.contains(p)) {
      pLookup(p).foldLeft(0.0) {
        case (acc, a) =>
          if (a.approval == approval) {
            acc + a.slot.duration.toMinutes
          } else {
            acc
          }
      }
    } else {
      0.0
    }
}
