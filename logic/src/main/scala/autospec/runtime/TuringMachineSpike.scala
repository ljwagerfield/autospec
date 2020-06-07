package autospec.runtime

import autospec.common.DistinctByKey
import autospec.common.turing.Machine
import autospec.common.turing.NextState.{Continue, Terminate}
import autospec.common.turing.TapeSymbol.{Input, Output}
import autospec.common.turing.TerminalState.{Accept, Reject}
import autospec.common.turing.Transition.{FromRightEnd, Normal}
import autospec.runtime.TuringMachineSpike.TuringMachineExamples.Binary.{One, Zero}
import autospec.runtime.TuringMachineSpike.TuringMachineExamples.{Example1, Example2}
import cats.Eq
import cats.implicits._

object TuringMachineSpike extends App {

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
    println(s"Valid $title: \n${machine.generate.take(20).toList.mkString("\n")}")

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
