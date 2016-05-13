package Core

import Constraints._
import smtlib.parser.Commands.DefineFun
import smtlib.parser.Terms.{SSymbol, SExpr, SNumeral}

object Schedule {
  def find(people: Set[Person], events: Set[Day], oldSchedule: Option[Schedule] = None) : Option[Schedule] = {
    // constants
    val MAXSLOTS  = 2
    val MINSLOTS  = 1
    val MAXDAYS   = 1
    val MINUTEEPS = 90

    // constraints
    val schedule = Timeslots(events)
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
        case DefineFun(fd) =>
          val slotname = fd.name

          if (slotmap.contains(slotname)) {
            fd.body match {
              case SNumeral(i) =>
                val personint = i.toInt
                val slot = slotmap(slotname)
                val person = peoplemap(personint)

                Some(Assignment(slot,person,Unapproved))
              case _ => None
            }
          } else {
            None
          }
        case _ => None
      }
    }

    val rejects: List[Assignment] = oldSchedule match {
      case Some(schedule) => schedule.assignments.flatMap { assn =>
        if (assn.a == Rejected) {
          Some(assn)
        } else {
          None
        }
      }.toList
      case None => List.empty
    }

    Schedule((assignments ::: rejects).sortWith { case (a1, a2) => a1.ds.start.isBefore(a2.ds.start)})
  }
}

case class Schedule(assignments: Seq[Assignment]) {

  val dsLookup = assignments.map { a => a.ds -> a }.toMap
  val pLookup : Map[Person,List[Assignment]] =
    assignments.foldLeft(assignments.map { a => a.p -> List[Assignment]() }.toMap){
      case (m,a) =>
        m + (a.p -> (a :: m(a.p)))
    }

  override def toString: String = {
    println("start,end,eventname,person,approval")
    assignments
      .sortWith { case (a1,a2) => a1.ds.start.isBefore(a2.ds.start) }
      .map { a =>
        "\"" + a.ds.start + "\",\"" + a.ds.end + "\",\"" + a.ds.eventname + "\",\"" + a.p.fname + " " + a.p.lname + "\",\"" + a.a + "\""
      }.mkString("\n")
  }

  def update(assignments: Seq[Assignment]) : Schedule = {
    ???
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
          if (a.a == approval) {
            acc + a.ds.duration.toMinutes
          } else {
            acc
          }
      }
    } else {
      0.0
    }
}
