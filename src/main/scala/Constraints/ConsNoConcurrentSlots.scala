package Constraints

import Core.Dateslot
import smtlib.parser.Commands._
import smtlib.parser.Terms._
import smtlib.theories.Core._
import smtlib.theories.Ints._

// Constraint #7: Do not assign a worker to concurrent timeslots
case class ConsNoConcurrentSlots(peoplemap: People#PeopleMap, slotmap: Timeslots#SlotMap) extends Constraint {
  private val constraints: Option[(DefineFun, List[Assert])] = init()

  private def init() : Option[(DefineFun,List[Assert])] = {
    val slotmap_inv = Util.invertMap(slotmap)
    val just_slots = slotmap.values.toSeq

    // find all overlapping slots
    val overlaps = Util
      .allNonSwapPairs[Dateslot](just_slots)
      .flatMap { case (t1,t2) =>
        if(t1.overlaps(t2)) {
          Some((t1,t2))
        } else {
          None
        }
      }

    val fname = SSymbol(this.getClass.getName)
    val arg_person = SortedVar(SSymbol("person"), IntSort())

    if (overlaps.nonEmpty) {
      // person should not be assigned to both t1 and t2
      val expr: Term =
        And(overlaps.map { case (t1,t2) =>
          Not(
            And(
              Equals(
                QualifiedIdentifier(SimpleIdentifier(slotmap_inv(t1))),
                QualifiedIdentifier(SimpleIdentifier(arg_person.name))
              ),
              Equals(
                QualifiedIdentifier(SimpleIdentifier(slotmap_inv(t2))),
                QualifiedIdentifier(SimpleIdentifier(arg_person.name))
              )
            )
          )
        })

      val assertions = peoplemap.map { case (person,_) =>
        Assert(
          FunctionApplication(QualifiedIdentifier(SimpleIdentifier(fname)),
            Seq(
              NumeralLit(person)
            )
          )
        )
      }.toList

      Some(
        DefineFun(FunDef(fname, Seq(arg_person), BoolSort(), expr)),
        assertions
      )
    } else {
      None
    }
  }

  def asserts: List[Assert] = constraints match {
    case Some((_,assertions)) => assertions
    case None => List()
  }
  def definition: List[Command] = constraints match {
    case Some((fdef,_)) => List(fdef)
    case None => List()
  }
}
