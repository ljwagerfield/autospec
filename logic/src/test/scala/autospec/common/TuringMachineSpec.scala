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
      * This test asserts the implementation of the generator does not linearly search all possible input sequences to
      * find the valid sequence(s), as this would be too slow for machines that have a small number of valid sequences
      * in comparison to the universe of all possible sequences.
      *
      * When thinking about time complexity, consider the set of all possible inputs (both valid and invalid) as a
      * "perfect m-ary tree" of height X and degree Y. X is the maximum sequence length we will generate and Y is the
      * number of symbols in the input alphabet. Each node represents an input sequence, and its ancestors are the set
      * of all its prefixes, with the root node being nil. The tree has `Y^X` nodes in it, and only a subset are valid.
      *
      * Worse-case scenario (demonstrated by this test): a large tree, with only one valid node (i.e. a password!).
      *
      * In our example, we have a 10-letter alphanumeric password, which gives us N = `62^10` (839 quadrillion) nodes...
      * but only one node is valid. If we start from the root node (nil), and append all possible input symbols, we will
      * arrive at each of the root's immediate children. If we repeat this process, we will effectively be performing a
      * breadth-first search, meaning we'd generate 839 quadrillion symbols in total. However, if we start from the
      * valid node, and traverse our way back to the root node by only prepending symbols that link us to prior states,
      * until we reach the start state, we would only need to generate O(logN) symbols (10 in this case).
      *
      * We verify O(logN) time complexity by running the generator for 2 seconds.
      * If it completes, it cannot be O(N) as N is 839 quadrillion... so it must be O(logN).
      */
    "generate valid sequences in O(logN) time" in {
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

  object Password {
    val chars: String    = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    val password: String = Random.alphanumeric.take(10).toList.mkString

    /**
      * Specifically designed to:
      * - Not terminate until Right End Marker (i.e. stay in a non-terminal state w/ transitions that match on it,
      *   even though we've deemed the password invalid).
      * - Causes the sequence to keep having new elements appended to it, if the generator was implemented using a
      *   naive linear search from the start state, thus causing `62^10` symbols to be generated.
      * - Use a random password, to avoid accidentally exercising an optimal path in the generator, i.e. if the
      *   generator followed a depth-first search starting from the head transition, and our password was 'aaaaaaaaaa',
      *   then it would generate a valid password on the first iteration.
      */
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
        length <- Gen.choose(0, 10)
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
      base   <- Gen.listOf(binaryGenerator)
      middle <- Gen.option(binaryGenerator)
    } yield base ::: middle.toList ::: base.reverse

    val mixedGenerator: Gen[List[Binary]] = Gen.oneOf(validGenerator, Gen.listOf(binaryGenerator))
  }

}
