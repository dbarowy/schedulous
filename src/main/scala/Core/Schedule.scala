package Core

import Constraints._
import smtlib.parser.Commands.{DefineFun, FunDef}
import smtlib.parser.Terms.{SExpr, SNumeral, SSymbol}

object Schedule {
  def find(setup: (People#PeopleMap, Timeslots#SlotMap, Option[Schedule]) => List[Constraint],
           people: Set[Person],
           events: Set[Day],
           oldSchedule: Option[Schedule] = None,
           debug: Boolean = true
          ) : Option[Schedule] = {

    // constraints
    val schedule = oldSchedule match {
      case None => Timeslots(events)
      case Some(s) => Timeslots(events, s.assignments.map { a => a.slotname -> a.slot }.toMap)
    }
    val participants = People(people)
    val constraints = setup(participants.peopleMap, schedule.slotMap, oldSchedule)

    // run solver
    val solver = Solver(schedule :: participants :: constraints, debug)

    // parse output
    solver.model match {
      case Some(mod) => Some(parseModel(mod, schedule.slotMap, participants.peopleMap, oldSchedule, debug))
      case None => None
    }
  }

  def parseModel(model: List[SExpr], slotmap: Timeslots#SlotMap, peoplemap: People#PeopleMap, oldSchedule: Option[Schedule], debug: Boolean = true) : Schedule = {
    if (debug) {
      model.foreach { sexpr => println(sexpr) }
    }

    val assignments: List[Assignment] = model.flatMap { expr =>
      expr match {
        case DefineFun(fd: FunDef) =>
          val slotname = fd.name

          if (slotmap.contains(slotname)) {
            val slot: Dateslot = slotmap(slotname)

            // create potential assignment if needed
            lazy val potentialAssn: Option[Assignment] = fd.body match {
              case SNumeral(i) =>
                val personint = i.toInt
                val person = try {
                  peoplemap(personint)
                } catch {
                  case e: Exception =>
                    val e1 = e
                    val pi = personint
                    val foo = peoplemap
                    throw e1
                }

                Some(Assignment(slotname, slot, person, Proposed))
              case _ => None
            }

            // emit previously-approved assignment if it exists
            oldSchedule match {
              case Some(sched) =>
                sched.getAssignmentFor(slot) match {
                  case Some(assn) =>
                    if (assn.approval == Approved) {
                      Some(assn)
                    } else {
                      potentialAssn
                    }
                  case None => potentialAssn
                }
              case None => potentialAssn
            }

          } else {
            None
          }
        case _ => None
      }
    }

    val rejects: List[Assignment] = oldSchedule match {
      case Some(schedule) => schedule.assignments.flatMap { assn =>
        if (assn.approval == Rejected) {
          Some(assn)
        } else {
          None
        }
      }.toList
      case None => List.empty
    }

    Schedule((assignments ::: rejects).sortWith { case (a1, a2) => a1.slot.start.isBefore(a2.slot.start)})
  }
}

case class Schedule(assignments: Seq[Assignment]) {

  private val dsLookup: Map[Dateslot, Assignment] = assignments.map { a => a.slot -> a }.toMap
  private val pLookup : Map[Person, List[Assignment]] =
    assignments.foldLeft(assignments.map { a => a.person -> List[Assignment]() }.toMap) {
      case (m,a) =>
        m + (a.person -> (a :: m(a.person)))
    }

  override def toString: String = {
    Assignment.header + "\n" +
    assignments
      .sortWith { case (a1,a2) =>
        a1.slot.start.isBefore(a2.slot.start)
      }
      .map(_.toString).mkString("\n")
  }

  def people : Set[Person] = assignments.map { assn => assn.person }.toSet
  def days : Set[Day] = {
    assignments
      .map { assn => assn.slot }
      .groupBy{ ds => ds.start.toLocalDate }
      .map { case (date,dslots) =>
        Day(date, dslots.map { ds =>
          Timeslot(ds.z3name, ds.prettyname, ds.role, ds.start.toLocalTime, ds.end.toLocalTime)
        }.toSet)
      }.toSet
  }

  def update(setup: (People#PeopleMap, Timeslots#SlotMap, Option[Schedule]) => List[Constraint],
             changes: Seq[Assignment]) : Option[Schedule] = {
    assert(changes.nonEmpty)

    // update assignments with set of changes
    val cMap = changes
      .filter(_.approval != Proposed)
      .map { a => a.id -> a }.toMap
    val assignments2 = assignments.map { a =>
      if (cMap.contains(a.id)) {
        cMap(a.id)
      } else {
        a
      }
    }

    Schedule.find(setup, people, days, Some(Schedule(assignments2)))
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

  def merge(s: Schedule) : Schedule = {
    val allAssn = (s.assignments.toList ::: this.assignments.toList)
      .sortWith { case (a1, a2) => a1.slot.start.isBefore(a2.slot.start)}
    Schedule(allAssn)
  }
}
