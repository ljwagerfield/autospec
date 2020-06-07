package autospec.common.turing

import autospec.common.DistinctByKey
import cats.Eq

/**
  * Turing Machine (technically a "Linear Bounded Automaton").
  *
  * Follows the Minsky 1967 definition:
  * - Each transition can both print a symbol and then optionally move the head L/R.
  * - Does not use "The Spurious Turing Convention" (i.e. F-Squares and E-Squares).
  *
  * Further, we introduce our own conventions:
  * - Tape size is finite and cannot grow beyond its initial size (aka a linear bounded automaton).
  * - Left/Right end markers are inserted before and after the input on the tape. These cannot be overwritten.
  * - Terminal states are hardcoded to 'Accept' and 'Reject'.
  * - If a transition doesn't exist for a state/symbol combination, we implicitly transition to 'Reject'.
  */
case class Machine[S: Eq, I, O](
  start: S,
  transitions: DistinctByKey[Transition[S, I, O]]
) {

  lazy val transitionsByState: Map[S, List[Transition[S, I, O]]] =
    transitions.value.values.groupBy(_.current).view.mapValues(_.toList).toMap

  def parse(input: List[I]): (TransitionFrom[S, I, O], Boolean) =
    MachineInstance
      .from(machine = this, input = input)
      .parseToEnd match { case (from, state) => from -> state.accepted }

  def generate: LazyList[List[I]] =
    MachineInstance
      .from(machine = this, input = Nil)
      .generateToEnd(isGenerating = true)

}
