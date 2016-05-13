package Core

trait Approval
case object Approved extends Approval
case object Rejected extends Approval
case object Unapproved extends Approval