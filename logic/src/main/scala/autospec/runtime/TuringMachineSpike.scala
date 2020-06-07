package autospec.runtime

import autospec.common.{DistinctByKey, HasKey}
import autospec.runtime.TuringMachineSpike.TuringMachineExamples.Binary.{One, Zero}
import autospec.runtime.TuringMachineSpike.TuringMachineExamples.{Example1, Example2}
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
import autospec.runtime.TuringMachineSpike.TuringMachineSchema.{Machine, MachineInstance, TransitionFrom}
import cats.implicits._
import cats.{Eq, Id}

object TuringMachineSpike extends App {

  object TuringMachineSchema {

    sealed trait Symbol[+I, +O]

    object Symbol {
      implicit def eq[I, O]: Eq[Symbol[I, O]] = Eq.fromUniversalEquals

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
      * - Terminal states are 'Accept' and 'Reject'
      * - If a transition doesn't exist for a state/symbol combination, we implicitly transition to 'Reject'.
      */
    case class Machine[S, I, O](
      start: S,
      transitions: DistinctByKey[Transition[S, I, O]]
    ) {

      lazy val transitionsByState: Map[S, List[Transition[S, I, O]]] =
        transitions.value.values.groupBy(_.current).view.mapValues(_.toList).toMap

    }

    case class MachineInstance[S: Eq, I, O](
      machine: Machine[S, I, O],
      current: S,
      tape: List[IOSymbol[I, O]],
      headIndex: Long
    ) {

      lazy val head: Symbol[I, O] = tape.get(headIndex).getOrElse {
        val isBeyondTape = headIndex >= tape.size
        if (isBeyondTape)
          RightEndMarker
        else
          LeftEndMarker
      }

      def parseToEnd: (TransitionFrom[S, I, O], TerminalState) =
        this.tailRecM[Id, (TransitionFrom[S, I, O], TerminalState)](_.parse)

      def parse: Either[MachineInstance[S, I, O], (TransitionFrom[S, I, O], TerminalState)] = {
        val defaultTransitionTo = TransitionTo[S, I, O](None, Terminate(Reject))
        val transitionFrom      = TransitionFrom[S, I, O](current, head)
        val transitionTo        = machine.transitions.value.get(transitionFrom).fold(defaultTransitionTo)(_.to)

        transitionTo.next match {
          case Terminate(terminalState) => (transitionFrom, terminalState).asRight
          case x @ Continue(nextState, _) =>
            copy(
              current   = nextState.getOrElse(current),
              headIndex = headIndex + x.moveHead.fold(0)(_.fold(_ => -1, _ => 1)),
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
                case Normal(_, input: Input[I], _, _) => input
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
        MachineInstance(machine, machine.start, input.map(Input.apply), 0)

    }

  }

  object TuringMachineParser {

    def isValidInput[S: Eq, I, O](machine: Machine[S, I, O], input: List[I]): (TransitionFrom[S, I, O], Boolean) =
      MachineInstance.from(machine, input).parseToEnd match {
        case (from, state) => from -> state.accepted
      }

  }

  object TuringMachineGenerator {

    def generateInput[S: Eq, I, O](machine: Machine[S, I, O]): LazyList[List[I]] =
      MachineInstance.from(machine, Nil).generateToEnd(true)

  }

  object TuringMachineExamples {
    sealed trait Binary

    object Binary {
      implicit val eq: Eq[Binary] = Eq.fromUniversalEquals

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
        implicit val eq: Eq[State] = Eq.fromUniversalEquals

        case object Start     extends State
        case object HaveZero  extends State
        case object HaveOne   extends State
        case object MatchZero extends State
        case object MatchOne  extends State
        case object Back      extends State
      }

      import State._

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

  def printSequence[S: Eq, I, O](title: String, machine: Machine[S, I, O]): Unit =
    println(s"Valid $title: \n${TuringMachineGenerator.generateInput(machine).take(20).toList.mkString("\n")}")

  printSequence("Alternating", Example1.machine)
  printSequence("Palindromes", Example2.machine)

//  val (lastTransition, isValid) =
//    TuringMachineParser.isValidInput(
//      Example2.machine,
//      Example2.invalidExample
//    )
//
//  println(s"Input: ${Example2.invalidExample}")
//
//  if (!isValid)
//    println(s"Rejected by: $lastTransition")
//
//  println(s"Valid: $isValid")

}
