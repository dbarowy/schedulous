package Core

object Approval {
  def fromString(approval: String) : Approval = {
    approval match {
      case "Approved" => Approved
      case "Rejected" => Rejected
      case "Proposed" => Proposed
      case "" => Proposed
    }
  }
}

trait Approval
case object Approved extends Approval
case object Rejected extends Approval
case object Proposed extends Approval