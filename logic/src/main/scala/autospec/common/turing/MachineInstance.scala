package autospec.common.turing

import autospec.common.turing.MachineState.{NonTerminalState, Reject, TerminalState}
import autospec.common.turing.TapeSymbol.{IOSymbol, Input, LeftEndMarker, Output, RightEndMarker}
import autospec.common.turing.Transition.Normal
import cats.implicits._
import cats.{Eq, Id}

import scala.collection.mutable

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

  private def parseToRightEndMarker: Either[MachineInstance[S, I, O], TerminalState] =
    this.tailRecM[Id, Either[MachineInstance[S, I, O], TerminalState]] { i =>
      if (i.head === RightEndMarker)
        i.asLeft.asRight
      else
        i.parse.map(_._2.asRight)
    }

  private def parse: Either[MachineInstance[S, I, O], (TransitionFrom[S, I, O], TerminalState)] = {
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

  def generateToEnd: LazyList[List[I]] = {
    val queue = mutable.Queue.empty[(List[I], MachineInstance[S, I, O])]

    def yieldNextInputSequence(machine: MachineInstance[S, I, O], inputHistory: List[I]): Option[List[I]] = {
      val (terminalState, alternatives) = machine.parseToEndAndSpawnAlternatives

      queue.enqueueAll(
        alternatives.map { case (input, machine) => (inputHistory :+ input) -> machine }
      )

      terminalState.fold(inputHistory.some, None)
    }

    (yieldNextInputSequence(machine = this, Nil) #:: LazyList.from(
      new Iterator[Option[List[I]]] {
        def hasNext: Boolean = queue.nonEmpty
        def next(): Option[List[I]] = {
          val (inputHistory, machine) = queue.dequeue()
          yieldNextInputSequence(machine, inputHistory)
        }
      }
    )).collect { case Some(inputSequence) => inputSequence }
  }

  private def parseToEndAndSpawnAlternatives: (TerminalState, List[(I, MachineInstance[S, I, O])]) =
    parseToRightEndMarker match {
      case Left(instance)  => instance.parseToEnd._2 -> instance.alternativeMachines
      case Right(terminal) => terminal               -> Nil
    }

  private def alternativeMachines: List[(I, MachineInstance[S, I, O])] = {
    val possibleExpansions =
      if (tape.size === MachineInstance.maxGeneratedTapeLength)
        Nil
      else
        machine.transitionsByState.getOrElse(current, Nil).collect {
          case Normal(_, input: Input[I], _, _, _) => input
        }

    possibleExpansions.map { input =>
      (
        input.value,
        copy(
          tape = tape :+ input
        )
      )
    }
  }

}

object MachineInstance {

  private val maxGeneratedTapeLength = 100

  def from[S: Eq, I, O](machine: Machine[S, I, O], input: List[I]): MachineInstance[S, I, O] =
    MachineInstance(
      machine   = machine,
      current   = machine.start,
      tape      = input.map(Input.apply),
      headIndex = 0
    )

}
