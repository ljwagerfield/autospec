package autospec.common.turing

import autospec.common.turing.TapeSymbol.{IOSymbol, Input, LeftEndMarker, Output, RightEndMarker}
import autospec.common.turing.MachineState.{Accept, NonTerminalState, Reject, TerminalState}
import autospec.common.turing.Transition.Normal
import cats.{Eq, Id}
import cats.implicits._

case class MachineInstance[S: Eq, I, O](
  machine: Machine[S, I, O],
  current: S,
  tape: List[IOSymbol[I, O]],
  headIndex: Long
) {

  lazy val head: TapeSymbol[I, O] = tape.get(headIndex).getOrElse {
    val isBeyondTape = headIndex >= tape.size
    if (isBeyondTape)
      RightEndMarker
    else
      LeftEndMarker
  }

  def parseToEnd: (TransitionFrom[S, I, O], TerminalState) =
    this.tailRecM[Id, (TransitionFrom[S, I, O], TerminalState)](_.parse)

  def parse: Either[MachineInstance[S, I, O], (TransitionFrom[S, I, O], TerminalState)] = {
    val defaultTransitionTo = TransitionTo[S, I, O](None, None, Some(Reject))
    val transitionFrom      = TransitionFrom[S, I, O](current, head)
    val transitionTo        = machine.transitions.value.get(transitionFrom).fold(defaultTransitionTo)(_.to)

    transitionTo.next.getOrElse(NonTerminalState(current)) match {
      case terminalState: TerminalState => (transitionFrom, terminalState).asRight
      case nextState: NonTerminalState[S] =>
        copy(
          current   = nextState.value,
          headIndex = headIndex + transitionTo.move.fold(0)(_.fold(_ => -1, _ => 1)),
          tape = transitionTo.write.fold(tape) { write =>
            tape.take(headIndex.toInt) ::: Output(write) :: tape.drop(headIndex.toInt + 1)
          }
        ).asLeft
    }
  }

  def generateToEnd(isGenerating: Boolean): LazyList[List[I]] =
    LazyList.from(generate(isGenerating)).flatMap {
      case (None, Left(isGenerating -> continue))        => continue.generateToEnd(isGenerating)
      case (Some(input), Left(isGenerating -> continue)) => continue.generateToEnd(isGenerating).map(input :: _)
      case (inputOpt, Right(Accept))                     => List(inputOpt.toList)
    }

  /**
    * Performs a single iteration in the process of attempting to find input sequences that satisfy the machine.
    * - Returns nil if the machine's initial tape configuration has now been determined as unacceptable.
    * - Returns one element if we're continuing with the same initial tape configuration.
    * - Returns multiple elements if we're exploring further different initial tape configurations.
    * Note: 'initial tape configuration' refers to the input sequence that was initially fed to the machine. Since
    * turing machines can only see the value under the head, we generate the 'initial tape configuration' as we go:
    * each time we hit the end of the tape, we add another element that would match a transition, up to a maximum
    * tape length defined by [[MachineInstance.maxGeneratedTapeLength]] to keep the process bounded.
    */
  def generate(isGenerating: Boolean): List[(Option[I], Either[(Boolean, MachineInstance[S, I, O]), Accept.type])] =
    if (head === RightEndMarker && isGenerating) {
      val possibleExpansions =
        if (tape.size === MachineInstance.maxGeneratedTapeLength)
          Nil
        else
          machine.transitionsByState.getOrElse(current, Nil).collect {
            case Normal(_, input: Input[I], _, _, _) => input
          }

      val alternativeMachines =
        possibleExpansions.map { input =>
          (
            copy(
              tape = tape :+ input
            ),
            true,
            Some(input.value)
          )
        }

      val allMachines = (this, false, None) :: alternativeMachines

      allMachines.flatMap {
        case (machine, isGenerating, generatedInput) => machine.parseForGenerator(isGenerating, generatedInput)
      }
    }
    else
      parseForGenerator(isGenerating, None)

  private def parseForGenerator(
    isGenerating: Boolean,
    generatedInput: Option[I]
  ): List[(Option[I], Either[(Boolean, MachineInstance[S, I, O]), Accept.type])] =
    parse match {
      case Left(continue)     => List(generatedInput -> Left(isGenerating -> continue))
      case Right(_ -> Accept) => List(generatedInput -> Right(Accept))
      case Right(_ -> Reject) => Nil
    }

}

object MachineInstance {

  private val maxGeneratedTapeLength = 4

  def from[S: Eq, I, O](machine: Machine[S, I, O], input: List[I]): MachineInstance[S, I, O] =
    MachineInstance(
      machine   = machine,
      current   = machine.start,
      tape      = input.map(Input.apply),
      headIndex = 0
    )

}
