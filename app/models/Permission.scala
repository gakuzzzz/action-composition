package models

sealed trait ActionType

case object Read extends ActionType
case object Write extends ActionType

sealed trait Permission {
  def executable(actionType: ActionType): Boolean
}

case object ReadOnly extends Permission {
  def executable(actionType: ActionType): Boolean = actionType == Read
}
case object WriteOnly extends Permission {
  def executable(actionType: ActionType): Boolean = actionType == Write
}
case object ReadWrite extends Permission {
  def executable(actionType: ActionType): Boolean =
    actionType == Read || actionType == Write
}
