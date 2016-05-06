package Core

import Constraints._
import smtlib.parser.Commands.DefineFun
import smtlib.parser.Terms.{SSymbol, SExpr, SNumeral}

object Schedule {
  def solve(people: Set[Person], events: Set[Day]) : Option[Schedule] = {
    // constants
    val MAXSLOTS  = 2
    val MINSLOTS  = 1
    val MAXDAYS   = 1
    val MINUTEEPS = 90

    // constraints
    val schedule = Timeslots(events)
    val participants = People(people)
    val c1 = ConsFillSlots(participants.peopleMap, schedule.slotMap)
    val c2 = ConsMaxSlots(MAXSLOTS, participants.peopleMap, schedule.slotMap)
    val c3 = ConsMinSlots(MINSLOTS, participants.peopleMap, schedule.slotMap)
    val c4 = ConsMaxDays(MAXDAYS, events, participants.peopleMap, schedule.slotMap)
    val c5 = ConsWorkload(participants.peopleMap, schedule.slotMap)
    val c6 = ConsAvgWorkload(MINUTEEPS, c5.name, participants.peopleMap, schedule.slotMap)
    val c7 = ConsNoConcurrentSlots(participants.peopleMap, schedule.slotMap)

    // solver
    val solver = Solver(Seq(schedule,participants,c1,c2,c3,c4,c5,c6,c7), debug = true)

    solver.model match {
      case Some(mod) => Some(parseModel(mod, schedule.slotMap, participants.peopleMap))
      case None => None
    }
  }

  def parseModel(model: List[SExpr], slotmap: Timeslots#SlotMap, peoplemap: People#PeopleMap) : Schedule = {
    val rx = """\(define-fun (.+) \(\) Int ([0-9]+)\)""".r

    val pairs = model.flatMap { expr =>
      expr match {
        case DefineFun(fd) =>
          val slotname = fd.name

          if (slotmap.contains(slotname)) {
            fd.body match {
              case SNumeral(i) =>
                val personint = i.toInt
                val slot = slotmap(slotname)
                val person = peoplemap(personint)

                Some(slot,person)
              case _ => None
            }
          } else {
            None
          }
        case _ => None
      }
    }

    Schedule(pairs)
  }
}

case class Schedule(pairings: Seq[(Dateslot,Person)]) {
  override def toString() : String = {
    println("start,end,eventname,person")
    pairings
      .sortWith { case ((d1,_),(d2,_)) => d1.start.isBefore(d2.start) }
      .map { case (d,p) =>
        "\"" + d.start + "\",\"" + d.end + "\",\"" + d.eventname + "\",\"" + p.fname + " " + p.lname + "\""
      }.mkString("\n")
  }
}
