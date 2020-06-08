package autospec.runtime

import autospec.common.DistinctByKey
import autospec.common.turing.TapeSymbol.{Input, Output}
import autospec.common.turing.Transition.{FromRightEnd, Normal}
import autospec.common.turing._
import autospec.runtime.TuringMachineSpike.TuringMachineExamples.Binary.{One, Zero}
import autospec.runtime.TuringMachineSpike.TuringMachineExamples.Example2
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
            FromRightEnd(None, noMove, accept),
            // Start of non-empty sequence.
            Normal(None, Input(Zero), noWrite, moveRight, nextState(Zero.some)),
            Normal(None, Input(One), noWrite, moveRight, nextState(One.some)),
            // Middle of non-empty sequence.
            Normal(Zero.some, Input(One), noWrite, moveRight, nextState(One.some)),
            Normal(One.some, Input(Zero), noWrite, moveRight, nextState(Zero.some)),
            // End of non-empty sequence.
            FromRightEnd(Zero.some, noMove, accept),
            FromRightEnd(One.some, noMove, accept)
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
            FromRightEnd(Start, noMove, accept),
            // Starting again, but nothing left to process.
            Normal(Start, Output(()), noWrite, noMove, accept),
            // Start of non-empty sequence.
            Normal(Start, Input(Zero), write(()), moveRight, nextState(HaveZero)),
            Normal(Start, Input(One), write(()), moveRight, nextState(HaveOne)),
            // Skip over other symbols, until we reach the end.
            Normal(HaveZero, Input(One), noWrite, moveRight, noStateChange),
            Normal(HaveZero, Input(Zero), noWrite, moveRight, noStateChange),
            Normal(HaveOne, Input(One), noWrite, moveRight, noStateChange),
            Normal(HaveOne, Input(Zero), noWrite, moveRight, noStateChange),
            // Reached the end.
            Normal(HaveZero, Output(()), noWrite, moveLeft, nextState(MatchZero)),
            Normal(HaveOne, Output(()), noWrite, moveLeft, nextState(MatchOne)),
            FromRightEnd(HaveZero, moveLeft, nextState(MatchZero)),
            FromRightEnd(HaveOne, moveLeft, nextState(MatchOne)),
            // Verify last character matches first character...
            // ... matches, so reset back to start and continue.
            Normal(MatchZero, Input(Zero), write(()), moveLeft, nextState(Back)),
            Normal(MatchOne, Input(One), write(()), moveLeft, nextState(Back)),
            // ... mismatches, so reject.
            Normal(MatchZero, Input(One), noWrite, noMove, reject),
            Normal(MatchOne, Input(Zero), noWrite, noMove, reject),
            // ... turns out we grabbed the last character, so accept it.
            Normal(MatchZero, Output(()), noWrite, noMove, accept),
            Normal(MatchOne, Output(()), noWrite, noMove, accept),
            // Continue going back to start.
            Normal(Back, Input(Zero), noWrite, moveLeft, noStateChange),
            Normal(Back, Input(One), noWrite, moveLeft, noStateChange),
            // Once at the start, repeat the process.
            Normal(Back, Output(()), noWrite, moveRight, nextState(Start))
          )
        )

      val validExample   = List(Zero, One, One, Zero)
      val invalidExample = List(Zero, One, One, Zero, Zero)
    }

  }

  def printSequence[S: Eq, I, O](title: String, machine: Machine[S, I, O]): Unit =
    println(s"Valid $title: \n${machine.generate.take(100).toList.mkString("\n")}")

  //printSequence("Alternating", Example1.machine)
  printSequence("Palindromes", Example2.machine)

}
