package autospec.common

import autospec.common.turing.TapeSymbol.{Input, Output}
import autospec.common.turing.Transition.{FromRightEnd, Normal}
import autospec.common.turing._
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

      verifyMachine(
        name    = "palindromes (type-2 grammar)",
        machine = Palindromes.machine,
        input   = Palindromes.mixedGenerator
      )(
        function = x => x.reverse == x
      )

      // Todo: type-1 grammar.
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
          FromRightEnd(password.length, noMove, accept)
            :: password.zipWithIndex.toList.flatMap {
              case (passwordChar, index) =>
                chars.map { char =>
                  Normal(index, Input(char), noWrite, moveRight, nextState(if (passwordChar == char) index + 1 else -1))
                }
            } ::: chars.toList.map(char => Normal(-1, Input(char), noWrite, moveRight, noStateChange))
        )
      )

  }

  /**
    * Alternating sequences are a type-3 grammar.
    *
    * They can be expressed by:
    * - Turing Machines
    * - Pushdown Automatons
    * - Finite State Machines (FSMs)
    */
  object AlternatingSequences {

    val machine: Machine[Option[Binary], Binary, Unit] =
      Machine(
        None,
        DistinctByKey[Transition[Option[Binary], Binary, Unit]](
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

  /**
    * Palindromes are a type-2 grammar.
    *
    * They can be expressed by:
    * - Turing Machines
    * - Pushdown Automatons
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

    val validGenerator: Gen[List[Binary]] = for {
      length <- Gen.choose(0, maxSequenceSize)
      base   <- Gen.listOfN((length - 1) / 2, binaryGenerator)
      middle <- Gen.option(binaryGenerator)
    } yield base ::: middle.toList ::: base.reverse

    val mixedGenerator: Gen[List[Binary]] = Gen.oneOf(validGenerator, Gen.listOf(binaryGenerator))
  }

}
