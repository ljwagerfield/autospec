package autospec.common

import autospec.common.turing.MachineState.{Accept, NonTerminalState, Reject}
import cats.implicits._

/**
  * Helpers for declaring turing machines.
  */
package object turing {
  val moveRight: Option[Right[Nothing, Unit]]                 = Right(()).some
  val moveLeft: Option[Left[Unit, Nothing]]                   = Left(()).some
  val accept: Option[Accept.type]                             = Accept.some
  val reject: Option[Reject.type]                             = Reject.some
  val noWrite: None.type                                      = None
  val noMove: None.type                                       = None
  val noStateChange: None.type                                = None
  def nextState[S](nextState: S): Option[NonTerminalState[S]] = NonTerminalState(nextState).some
  def write[O](outputSymbol: O): Option[O]                    = outputSymbol.some
}
