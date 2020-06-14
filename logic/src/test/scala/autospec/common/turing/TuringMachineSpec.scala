package autospec.common.turing

import autospec.common.DistinctByKey
import autospec.common.turing.MachineState.{Accept, Reject}
import autospec.common.turing.TapeSymbol.{Input, RightEndMarker}
import autospec.common.turing.Transition.{FromMiddle, FromRightEnd}
import cats.Eq
import cats.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import org.scalacheck.Gen

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.Random

class TuringMachineSpec extends TuringMachineSpecBase {

  "Turing Machines" should {
    "be capable of expressing all grammars, such as..." when {
      // Todo: type-0 grammar.

      // Todo: type-1 grammar.

      verifyMachine(
        name    = "palindromes (type-2 grammar)",
        machine = Palindromes.machine,
        input   = Palindromes.mixedGenerator
      )(
        function = x => x.reverse == x
      )

      verifyMachine(
        name    = "alternating sequences (type-3 grammar)",
        machine = AlternatingSequences.machine,
        input   = AlternatingSequences.mixedGenerator
      )(
        function = _.foldLeft(true -> (None: Option[Binary])) { (accum, element) =>
          val (accept, previous) = accum
          (
            accept && !previous.contains_(element),
            Some(element)
          )
        }._1
      )
    }

    /**
      * The search space for input sequences (i.e. the universal set from which a valid input sequence is found) can be
      * large, so it's important our method for finding a valid sequence is faster than O(N) for all machines. Formally,
      * N is `X^Y`, where X is the input alphabet size, and Y is the input sequence size.
      *
      * We verify O(logN) time complexity by running the generator for a machine where N is 839 quadrillion `(62^10)`,
      * and only one valid input sequence exists in the whole set.
      *
      * If it completes within 2 seconds, it cannot be O(N)... so it must be O(logN).
      */
    "generates a valid sequence in O(logN) time, where N is X^Y, X is the input alphabet size, and Y is the input sequence size" in {
      implicit val scheduler: Scheduler = Scheduler.traced
      try Task.evalAsync {
        Password.machine.generate.head.mkString shouldBe Password.password
      }.runSyncUnsafe(2.seconds)
      catch {
        case e: TimeoutException =>
          throw new Exception("Turing machine sequence generator does not appear to be a O(logN) algorithm!", e)
      }
    }
  }

  /**
    * Hardcoded password (a machine that only matches against one baked-in sequence of input symbols).
    *
    * Represents a machine that is not feasibly linearly searchable from the 'start' node to the 'accept' node, as it
    * has only one valid input sequence in a huge universe of possible input sequences.
    *
    * Specifically designed to:
    * - Not terminate until Right End Marker (i.e. regardless of if we've already determined the input is invalid).
    * - Causes the sequence to keep having new elements appended to it, assuming the generator was implemented using a
    *   naive linear search from the start state, which would cause `62^10` symbols to be generated.
    * - Uses a random password, to avoid accidentally exercising an optimal path in the generator, i.e. if the
    *   generator followed a depth-first search starting from the head transition, and our password was 'aaaaaaaaaa',
    *   then it would generate a valid password on the first iteration.
    */
  object Password {
    val chars: String    = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    val password: String = Random.alphanumeric.take(10).toList.mkString

    val machine: Machine[Int, Char, Unit] =
      Machine(
        0,
        DistinctByKey(
          FromRightEnd(password.length, None, accept)
            :: password.zipWithIndex.toList.flatMap {
              case (passwordChar, index) =>
                chars.map { char =>
                  FromMiddle(
                    index,
                    Input(char),
                    noWrite,
                    right,
                    nextState(if (passwordChar == char) index + 1 else -1)
                  )
                }
            } ::: chars.toList.map(char => FromMiddle(-1, Input(char), noWrite, right, noStateChange))
        )
      )

  }

  /**
    * Bach Sequences are a type-1 grammar.
    *
    * They can be expressed by:
    * - Turing Machines
    *
    * Examples (requires each symbol to appear same number of times, in any order):
    * - 10
    * - 1100
    * - 1001
    *
    * See:
    * - Pullum, Geoffrey K. (1983). Context-freeness and the computer processing of human languages. Proc. 21st An
    */
  object BachSequences {
    // Approach:
    // Pick up a symbol
    // Delete it
    // Scan list and delete next occurrence of other symbol
    // If cannot find an occurrence of other symbol, abort
    // Go back to start and do it again.
  }

  /**
    * Palindromes are a type-2 grammar.
    *
    * They can be expressed by:
    * - Turing Machines
    * - Pushdown Automatons
    *
    * Examples:
    * - 1
    * - 110011
    * - 0010100
    */
  object Palindromes {
    implicit val eq: Eq[State] = Eq.fromUniversalEquals

    sealed trait State
    case object Start     extends State
    case object HaveZero  extends State
    case object HaveOne   extends State
    case object MatchZero extends State
    case object MatchOne  extends State
    case object Back      extends State

    val machine: Machine[State, Binary, Unit] =
      Machine(
        Start,
        DistinctByKey(
          (Start, RightEndMarker)    -> (Accept, RightEndMarker, hold),    // Empty sequence.
          (Start, Zero)              -> (HaveZero, (), right),             // Start of sequence.
          (Start, One)               -> (HaveOne, (), right),              // ...
          (Start, ())                -> (Accept, (), hold),                // Starting again, but nothing left to process.
          (HaveZero, One)            -> (HaveZero, One, right),            // Skip over other symbols, until we reach the end.
          (HaveZero, Zero)           -> (HaveZero, Zero, right),           // ...
          (HaveOne, One)             -> (HaveOne, One, right),             // ...
          (HaveOne, Zero)            -> (HaveOne, Zero, right),            // ...
          (HaveZero, ())             -> (MatchZero, (), left),             // Reached the end.
          (HaveZero, RightEndMarker) -> (MatchZero, RightEndMarker, left), // ...
          (HaveOne, ())              -> (MatchOne, (), left),              // ...
          (HaveOne, RightEndMarker)  -> (MatchOne, RightEndMarker, left),  // ...
          (MatchZero, Zero)          -> (Back, (), left),                  // Verify last character matches first character.
          (MatchZero, One)           -> (Reject, One, hold),               // ...
          (MatchZero, ())            -> (Accept, (), hold),                // ...
          (MatchOne, One)            -> (Back, (), left),                  // ...
          (MatchOne, Zero)           -> (Reject, Zero, hold),              // ...
          (MatchOne, ())             -> (Accept, (), hold),                // ...
          (Back, Zero)               -> (Back, Zero, left),                // Go back to start.
          (Back, One)                -> (Back, One, left),                 // ...
          (Back, ())                 -> (Start, (), right)                 // ...
        )
      )

    val validGenerator: Gen[List[Binary]] = for {
      length <- Gen.choose(0, maxSequenceSize)
      base   <- Gen.listOfN((length - 1) / 2, binaryGenerator)
      middle <- Gen.option(binaryGenerator)
    } yield base ::: middle.toList ::: base.reverse

    val mixedGenerator: Gen[List[Binary]] = Gen.oneOf(validGenerator, Gen.listOf(binaryGenerator))
  }

  /**
    * Alternating sequences are a type-3 grammar.
    *
    * They can be expressed by:
    * - Turing Machines
    * - Pushdown Automatons
    * - Finite State Machines (FSMs)
    *
    * Examples:
    * - 1
    * - 1010
    * - 01010
    */
  object AlternatingSequences {

    val machine: Machine[Option[Binary], Binary, Unit] =
      Machine(
        None,
        DistinctByKey(
          (None, RightEndMarker)      -> (Accept, RightEndMarker, hold), // Empty sequence.
          (None, Zero)                -> (Zero.some, Zero, right),       // Start of sequence.
          (None, One)                 -> (One.some, One, right),         // ...
          (Zero.some, One)            -> (One.some, One, right),         // Middle of sequence.
          (One.some, Zero)            -> (Zero.some, Zero, right),       // ...
          (Zero.some, RightEndMarker) -> (Accept, RightEndMarker, hold), // End of non-empty sequence.
          (One.some, RightEndMarker)  -> (Accept, RightEndMarker, hold)  // ...
        )
      )

    val validGenerator: Gen[List[Binary]] =
      for {
        seed   <- binaryGenerator
        length <- Gen.choose(0, maxSequenceSize)
      } yield (0 until length).toList.map { x =>
        if (x % 2 == 0)
          seed
        else
          seed.flip
      }

    val mixedGenerator: Gen[List[Binary]] = Gen.oneOf(validGenerator, Gen.listOf(binaryGenerator))

  }

}
