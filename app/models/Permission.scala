package models

sealed trait ActionType

case object Read extends ActionType
case object Write extends ActionType

sealed trait Permission {
  def executable(actionType: ActionType): Boolean = (this, actionType) match {
    case (ReadOnly, Read) => true
    case (WriteOnly, Write) => true
    case (ReadWrite, _) => true
    case _ => false
  }
}

case object ReadOnly extends Permission
case object WriteOnly extends Permission
case object ReadWrite extends Permission
