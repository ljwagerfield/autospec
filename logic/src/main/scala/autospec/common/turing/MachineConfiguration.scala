package autospec.common.turing

import autospec.common.turing.MachineState.{Accept, NonTerminalState, Reject, TerminalState}
import autospec.common.turing.TapeSymbol.{IOSymbol, Input, LeftEndMarker, Output, RightEndMarker}
import cats.implicits._
import cats.{Eq, Id}

import scala.collection.mutable

case class MachineConfiguration[S: Eq, I, O](
  machine: Machine[S, I, O],
  state: MachineState[S],
  tape: List[IOSymbol[I, O]],
  isLeftBounded: Boolean,  // If true, implies a 'left end marker' exists on the tape at position `-1`.
  isRightBounded: Boolean, // If true, implies a 'right end marker' exists on the tape at position `tape.size`.
  head: Long
) {

  private lazy val headSymbol: Option[TapeSymbol[I, O]] = tape.get(head).orElse {
    Some(headSymbolIfTapeOverflowed).filter {
      case LeftEndMarker  => isLeftBounded
      case RightEndMarker => isRightBounded
    }
  }

  private lazy val headSymbolIfBounded: TapeSymbol[I, O] = tape.get(head).getOrElse(headSymbolIfTapeOverflowed)

  private def headSymbolIfTapeOverflowed: TapeSymbol.EndMarker = {
    require(head < 0 || head >= tape.size)
    val isBeyondTape = head >= tape.size
    if (isBeyondTape)
      RightEndMarker
    else
      LeftEndMarker
  }

  def parseToEnd: TerminalState =
    this.tailRecM[Id, TerminalState](_.parse)

  private def parse: Either[MachineConfiguration[S, I, O], TerminalState] =
    state match {
      case s: TerminalState =>
        s.asRight
      case NonTerminalState(s) =>
        val transitionFrom      = TransitionFrom[S, I, O](s, headSymbolIfBounded)
        val transitionToDefault = TransitionTo[S, I, O](transitionFrom.read, None, Reject)
        val transitionTo        = machine.transitions.value.get(transitionFrom).fold(transitionToDefault)(_.to)
        copy(
          state = transitionTo.next,
          head  = head + transitionTo.move.fold(0)(_.fold(_ => -1, _ => 1)),
          tape = Some(transitionTo.leave)
            .filter(_ =!= transitionFrom.read)
            .collect { case x: Output[O] => x }
            .fold(tape) {
              write => tape.take(head.toInt) ::: write :: tape.drop(head.toInt + 1)
            }
        ).asLeft
    }

  def generateToEnd: LazyList[List[I]] = {
    val queue = mutable.Queue.empty[MachineConfiguration[S, I, O]]

    def yieldNextInputSequence(machine: MachineConfiguration[S, I, O]): Option[List[I]] = {
      val (inputSequence, alternatives) = machine.generate

      queue.enqueueAll(alternatives)

      inputSequence
    }

    (yieldNextInputSequence(machine = this) #:: LazyList.from(
      new Iterator[Option[List[I]]] {
        def hasNext: Boolean = queue.nonEmpty
        def next(): Option[List[I]] = {
          val machine = queue.dequeue()
          yieldNextInputSequence(machine)
        }
      }
    )).collect { case Some(inputSequence) => inputSequence }
  }

  private def generate: (Option[List[I]], List[MachineConfiguration[S, I, O]]) = {
    val validInput =
      if (state === NonTerminalState(machine.start)) {
        val inputs    = tape.collect { case Input(input) => input }
        val allInputs = inputs.size === tape.size
        Some(inputs).filter(_ => allInputs)
      }
      else
        None

    val possiblePreviousTransitions = machine.transitionsToState.getOrElse(state, Nil)
    val leftSymbol                  = copy(head = head - 1).headSymbol
    val rightSymbol                 = copy(head = head + 1).headSymbol
    val canWriteLeft                = headSymbol.forall(_ =!= LeftEndMarker)
    val canWriteRight               = headSymbol.forall(_ =!= RightEndMarker)

    val validPreviousTransitions =
      possiblePreviousTransitions.filter { transition =>
        transition.to.move match {
          case None => headSymbol.forall(_ === transition.to.leave)
          // Intentionally swap left/right here: when a previous transition moves right, it means
          // it wrote to the left of the future head (i.e. current head) and visa-versa.
          case Some(Right(())) => canWriteLeft && leftSymbol.forall(_ === transition.to.leave)
          case Some(Left(()))  => canWriteRight && rightSymbol.forall(_ === transition.to.leave)
        }
      }

    val validPreviousConfigurations =
      validPreviousTransitions.map { transition =>
        val nextHead = head + transition.to.move.fold(0)(_.fold(_ => 1, _ => -1))
        val partialUpdate = copy(
          state = NonTerminalState(transition.current),
          head  = nextHead
        )
        transition.read match {
          case LeftEndMarker =>
            partialUpdate.copy(
              isLeftBounded = true
            )
          case RightEndMarker =>
            partialUpdate.copy(
              isRightBounded = true
            )
          case symbol: IOSymbol[I, O] =>
            val nextTape = tape.take(nextHead.toInt) ::: symbol :: tape.drop(nextHead.toInt + 1)
            partialUpdate.copy(
              tape = nextTape,
              head = Math.min( // For when we expand right AND the tape is empty (initialHead=0, nextHead=1).
                nextTape.size - 1L,
                Math.max(0L, nextHead) // For when we expand to the left (initialHead=0, nextHead=-1).
              )
            )
        }
      }

    validInput -> validPreviousConfigurations
  }

}

object MachineConfiguration {

  def forParsing[S: Eq, I, O](machine: Machine[S, I, O], input: List[I]): MachineConfiguration[S, I, O] =
    MachineConfiguration(
      machine        = machine,
      state          = NonTerminalState(machine.start),
      tape           = input.map(Input.apply),
      head           = 0,
      isLeftBounded  = true,
      isRightBounded = true
    )

  def forGenerating[S: Eq, I, O](machine: Machine[S, I, O]): MachineConfiguration[S, I, O] =
    MachineConfiguration(
      machine        = machine,
      state          = Accept,
      tape           = Nil,
      head           = 0,
      isLeftBounded  = false,
      isRightBounded = false
    )

}
