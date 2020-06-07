package autospec.common.turing

import autospec.common.turing.MachineState.{Accept, NonTerminalState, Reject}

sealed trait MachineState[+S] {

  def accepted: Boolean =
    this match {
      case Accept                          => true
      case Reject | _: NonTerminalState[_] => false
    }

}

object MachineState {

  sealed trait TerminalState extends MachineState[Nothing] {

    def fold[A](accept: => A, reject: => A): A =
      this match {
        case Accept => accept
        case Reject => reject
      }

  }

  case object Accept                        extends TerminalState
  case object Reject                        extends TerminalState
  case class NonTerminalState[+S](value: S) extends MachineState[S]
}
