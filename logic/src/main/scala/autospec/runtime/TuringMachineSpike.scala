package autospec.runtime

import autospec.common.{DistinctByKey, HasKey}
import autospec.runtime.TuringMachineSpike.TuringMachineExamples.Binary.{One, Zero}
import autospec.runtime.TuringMachineSpike.TuringMachineExamples.Example2
import autospec.runtime.TuringMachineSpike.TuringMachineExamples.Example2.State.{
  Back,
  HaveOne,
  HaveZero,
  MatchOne,
  MatchZero,
  Start
}
import autospec.runtime.TuringMachineSpike.TuringMachineSchema.{Machine, TerminalState, TransitionFrom, TransitionTo}
import autospec.runtime.TuringMachineSpike.TuringMachineSchema.NextState.{Continue, Terminate}
import autospec.runtime.TuringMachineSpike.TuringMachineSchema.Symbol.{
  IOSymbol,
  Input,
  LeftEndMarker,
  Output,
  RightEndMarker
}
import autospec.runtime.TuringMachineSpike.TuringMachineSchema.TerminalState.{Accept, Reject}
import autospec.runtime.TuringMachineSpike.TuringMachineSchema.Transition.{FromRightEnd, Normal}
import cats.Id
import cats.implicits._

object TuringMachineSpike extends App {

  object TuringMachineSchema {

    sealed trait Symbol[+I, +O]

    object Symbol {
      sealed trait IOSymbol[+I, +O]  extends Symbol[I, O]
      case class Input[I](value: I)  extends IOSymbol[I, Nothing]
      case class Output[O](value: O) extends IOSymbol[Nothing, O]

      sealed trait EndMarker     extends Symbol[Nothing, Nothing]
      case object LeftEndMarker  extends EndMarker
      case object RightEndMarker extends EndMarker
    }

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

    sealed trait Transition[+S, +I, +O] {
      def from: TransitionFrom[S, I, O] = TransitionFrom(current, read)
      def to: TransitionTo[S, I, O]     = TransitionTo(write, next)
      def current: S
      def read: Symbol[I, O]
      def write: Option[O]
      def next: NextState[S, _ <: Either[Unit, Unit]]
    }

    case class TransitionFrom[+S, +I, +O](current: S, read: Symbol[I, O])
    case class TransitionTo[+S, +I, +O](write: Option[O], next: NextState[S, _ <: Either[Unit, Unit]])

    object Transition {

      implicit def hasKey[S, I, O]: HasKey[TransitionFrom[S, I, O], Transition[S, I, O]] =
        new HasKey[TransitionFrom[S, I, O], Transition[S, I, O]] {
          override def key: Transition[S, I, O] => TransitionFrom[S, I, O] = _.from
        }

      case class Normal[+S, I, O](
        current: S,
        read: IOSymbol[I, O],
        write: Option[O],
        next: NextState[S, Either[Unit, Unit]]
      ) extends Transition[S, I, O]

      case class FromLeftEnd[S, I, O](current: S, next: NextState[S, Right[Unit, Unit]]) extends Transition[S, I, O] {
        override def read: Symbol[I, O] = LeftEndMarker
        override def write: Option[O]   = None
      }

      case class FromRightEnd[S, I, O](current: S, next: NextState[S, Left[Unit, Unit]]) extends Transition[S, I, O] {
        override def read: Symbol[I, O] = RightEndMarker
        override def write: Option[O]   = None
      }

    }

    sealed trait NextState[+S, +M]

    object NextState {
      case class Terminate(nextState: TerminalState)                       extends NextState[Nothing, Nothing]
      case class Continue[S, M](nextState: Option[S], moveHead: Option[M]) extends NextState[S, M]

      object Continue {
        def right[S](nextState: S): Continue[S, Right[Unit, Unit]] = Continue(Some(nextState), Some(Right(())))
        def right[S]: Continue[S, Right[Unit, Unit]]               = Continue(None, Some(Right(())))
        def left[S](nextState: S): Continue[S, Left[Unit, Unit]]   = Continue(Some(nextState), Some(Left(())))
        def left[S]: Continue[S, Left[Unit, Unit]]                 = Continue(None, Some(Left(())))
        def here[S](nextState: S): Continue[S, Nothing]            = Continue(Some(nextState), None)
      }

    }

    case class Machine[S, I, O](
      start: S,
      transitions: DistinctByKey[Transition[S, I, O]]
    )

  }

  object TuringMachineParser {

    private case class MachineInstance[S, I, O](
      machine: Machine[S, I, O],
      current: S,
      tape: List[IOSymbol[I, O]],
      headIndex: Long
    ) {

      def next: Either[(TransitionFrom[S, I, O], TerminalState), MachineInstance[S, I, O]] = {
        val head = tape.get(headIndex).getOrElse {
          val isBeyondTape = headIndex >= tape.size
          if (isBeyondTape)
            RightEndMarker
          else
            LeftEndMarker
        }

        val defaultTransitionTo = TransitionTo[S, I, O](None, Terminate(Reject))
        val transitionFrom      = TransitionFrom[S, I, O](current, head)
        val transitionTo        = machine.transitions.value.get(transitionFrom).fold(defaultTransitionTo)(_.to)

        transitionTo.next match {
          case Terminate(terminalState) => (transitionFrom, terminalState).asLeft
          case Continue(nextState, moveHead) =>
            copy(
              current   = nextState.getOrElse(current),
              headIndex = headIndex + moveHead.fold(0)(_.fold(_ => -1, _ => 1)),
              tape = transitionTo.write.fold(tape) { write =>
                tape.take(headIndex.toInt) ::: Output(write) :: tape.drop(headIndex.toInt + 1)
              }
            ).asRight
        }

      }

      def nextUntilTerminal: (TransitionFrom[S, I, O], TerminalState) =
        this.tailRecM[Id, (TransitionFrom[S, I, O], TerminalState)](_.next.swap)

    }

    private object MachineInstance {

      def from[S, I, O](machine: Machine[S, I, O], input: List[I]): MachineInstance[S, I, O] =
        MachineInstance(machine, machine.start, input.map(Input.apply), 0)

    }

    def isValidInput[S, I, O](machine: Machine[S, I, O], input: List[I]): (TransitionFrom[S, I, O], Boolean) =
      MachineInstance.from(machine, input).nextUntilTerminal match {
        case (from, state) => from -> state.accepted
      }

  }

  object TuringMachineExamples {
    sealed trait Binary

    object Binary {
      case object One  extends Binary
      case object Zero extends Binary
    }

    /**
      * Alternating 0s and 1s
      */
    object Example1 {

      val machine: Machine[Option[Binary], Binary, Unit] =
        Machine(
          None,
          DistinctByKey(
            // Empty sequence.
            FromRightEnd(None, Terminate(Accept)),
            // Start of non-empty sequence.
            Normal(None, Input(Zero), None, Continue.right(Some(Zero))),
            Normal(None, Input(One), None, Continue.right(Some(One))),
            // Middle of non-empty sequence.
            Normal(Some(Zero), Input(One), None, Continue.right(Some(One))),
            Normal(Some(One), Input(Zero), None, Continue.right(Some(Zero))),
            // End of non-empty sequence.
            FromRightEnd(Some(Zero), Terminate(Accept)),
            FromRightEnd(Some(One), Terminate(Accept))
          )
        )

      val validExample   = List(Zero, One, Zero, One, Zero)
      val invalidExample = List(Zero, One, Zero, Zero, One)
    }

    /**
      * Palindrome
      */
    object Example2 {

      sealed trait State

      object State {
        case object Start     extends State
        case object HaveZero  extends State
        case object HaveOne   extends State
        case object MatchZero extends State
        case object MatchOne  extends State
        case object Back      extends State
      }

      val machine: Machine[State, Binary, Unit] =
        Machine(
          Start,
          DistinctByKey(
            // Empty sequence.
            FromRightEnd(Start, Terminate(Accept)),
            // Starting again, but nothing left to process.
            Normal(Start, Output(()), None, Terminate(Accept)),
            // Start of non-empty sequence.
            Normal(Start, Input(Zero), Some(()), Continue.right(HaveZero)),
            Normal(Start, Input(One), Some(()), Continue.right(HaveOne)),
            // Skip over other symbols, until we reach the end.
            Normal(HaveZero, Input(One), None, Continue.right),
            Normal(HaveZero, Input(Zero), None, Continue.right),
            Normal(HaveOne, Input(One), None, Continue.right),
            Normal(HaveOne, Input(Zero), None, Continue.right),
            // Reached the end.
            Normal(HaveZero, Output(()), None, Continue.left(MatchZero)),
            Normal(HaveOne, Output(()), None, Continue.left(MatchOne)),
            FromRightEnd(HaveZero, Continue.left(MatchZero)),
            FromRightEnd(HaveOne, Continue.left(MatchOne)),
            // Verify last character matches first character...
            // ... matches, so reset back to start and continue.
            Normal(MatchZero, Input(Zero), Some(()), Continue.left(Back)),
            Normal(MatchOne, Input(One), Some(()), Continue.left(Back)),
            // ... mismatches, so reject.
            Normal(MatchZero, Input(One), None, Terminate(Reject)),
            Normal(MatchOne, Input(Zero), None, Terminate(Reject)),
            // ... turns out we grabbed the last character, so accept it.
            Normal(MatchZero, Output(()), None, Terminate(Accept)),
            Normal(MatchOne, Output(()), None, Terminate(Accept)),
            // Continue going back to start.
            Normal(Back, Input(Zero), None, Continue.left),
            Normal(Back, Input(One), None, Continue.left),
            // Once at the start, repeat the process.
            Normal(Back, Output(()), None, Continue.right(Start))
          )
        )

      val validExample   = List(Zero, One, One, Zero)
      val invalidExample = List(Zero, One, One, Zero, Zero)
    }

  }

  val (lastTransition, isValid) =
    TuringMachineParser.isValidInput(
      Example2.machine,
      Example2.validExample
    )

  if (!isValid)
    println(s"Rejected by: $lastTransition")
  println(s"Valid: $isValid")

  // Todo: build a 'generator' that starts from 'start' and traverses all paths to 'end', paying respect to
  //  the direction we've travelled in (left, right, nowhere) and the input symbols we've relied on, in order
  //  to generate a valid sequence. Note: 'Binary' will be 'UnboundRelation' in future... no it won't... as the
  //  relation may be dependent on something more dynamic, e.g. the last number.
}
