package Constraints

import smtlib.parser.Commands.{Assert, Command}

trait Constraint {
  def asserts : List[Assert]
  def definition : List[Command]
}
