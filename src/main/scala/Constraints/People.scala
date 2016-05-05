package Constraints

import Core.Person
import smtlib.parser.Commands.{Assert, Command}

case class People(people: Set[Person]) extends Constraint {
  type PeopleMap = Map[Int,Person]

  private val peoplemap =
    people.zipWithIndex.map { case (person,i) =>
      i -> person
    }.toMap

  def asserts: List[Assert] = List.empty
  def definition: List[Command] = List.empty
  def peopleMap: PeopleMap = peoplemap
}
