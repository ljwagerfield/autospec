package autospec.common.turing

import autospec.common.turing.TerminalState.{Accept, Reject}

sealed trait TerminalState {

  def accepted: Boolean =
    this match {
      case Accept => true
      case Reject => false
    }

  def rejected: Boolean = !accepted

}

object TerminalState {
  case object Accept extends TerminalState
  case object Reject extends TerminalState
}
