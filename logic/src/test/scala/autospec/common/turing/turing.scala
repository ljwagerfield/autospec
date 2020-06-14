package autospec.common

import autospec.common.turing.MachineState.{Accept, NonTerminalState, Reject, TerminalState}
import autospec.common.turing.TapeSymbol.{Input, Output, RightEndMarker}
import autospec.common.turing.Transition.{FromMiddle, FromRightEnd}
import cats.implicits._

/**
  * Helpers for declaring turing machines.
  */
package object turing {
  val right: Option[Right[Nothing, Unit]]                     = Right(()).some
  val left: Option[Left[Unit, Nothing]]                       = Left(()).some
  val hold: None.type                                         = None
  val accept: Option[Accept.type]                             = Accept.some
  val reject: Option[Reject.type]                             = Reject.some
  val noWrite: None.type                                      = None
  val noStateChange: None.type                                = None
  def nextState[S](nextState: S): Option[NonTerminalState[S]] = NonTerminalState(nextState).some
  def write[O](outputSymbol: O): Option[O]                    = outputSymbol.some

  implicit def tupledRuleREM[S, I, O](
    rule: ((S, RightEndMarker.type), (S, RightEndMarker.type, Option[Left[Unit, Unit]]))
  ): Transition[S, I, O] = {
    val ((fromState, _), (toState, _, move)) = rule
    FromRightEnd(fromState, move, Some(NonTerminalState(toState)))
  }

  implicit def tupledRuleREMTerminal[S, I, O](
    rule: ((S, RightEndMarker.type), (TerminalState, RightEndMarker.type, Option[Left[Unit, Unit]]))
  ): Transition[S, I, O] = {
    val ((fromState, _), (toState, _, move)) = rule
    FromRightEnd(fromState, move, Some(toState))
  }

  implicit def tupledRuleOO[S, I, O](rule: ((S, O), (S, O, Option[Either[Unit, Unit]]))): Transition[S, I, O] = {
    val ((fromState, read), (toState, write, move)) = rule
    FromMiddle(fromState, Output(read), Some(write), move, Some(NonTerminalState(toState)))
  }

  implicit def tupledRuleOOTerminal[S, I, O](
    rule: ((S, O), (TerminalState, O, Option[Either[Unit, Unit]]))
  ): Transition[S, I, O] = {
    val ((fromState, read), (toState, write, move)) = rule
    FromMiddle(fromState, Output(read), Some(write), move, Some(toState))
  }

  implicit def tupledRuleII[S, I, O](rule: ((S, I), (S, I, Option[Either[Unit, Unit]]))): Transition[S, I, O] = {
    val ((fromState, read), (toState, write, move)) = rule
    require(read == write, "Input symbols cannot be written, only read.")
    FromMiddle(fromState, Input(read), None, move, Some(NonTerminalState(toState)))
  }

  implicit def tupledRuleIITerminal[S, I, O](
    rule: ((S, I), (TerminalState, I, Option[Either[Unit, Unit]]))
  ): Transition[S, I, O] = {
    val ((fromState, read), (toState, write, move)) = rule
    require(read == write, "Input symbols cannot be written, only read.")
    FromMiddle(fromState, Input(read), None, move, Some(toState))
  }

  implicit def tupledRuleIO[S, I, O](rule: ((S, I), (S, O, Option[Either[Unit, Unit]]))): Transition[S, I, O] = {
    val ((fromState, read), (toState, write, move)) = rule
    FromMiddle(fromState, Input(read), Some(write), move, Some(NonTerminalState(toState)))
  }

}
