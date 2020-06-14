package autospec.common

import autospec.BaseSpec
import autospec.common.turing.Machine
import cats.Eq
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatestplus.scalacheck.Checkers

abstract class TuringMachineSpecBase extends BaseSpec with Checkers {
  implicit val eq: Eq[Binary]          = Eq.fromUniversalEquals
  implicit val eq2: Eq[Option[Binary]] = Eq.fromUniversalEquals
  val binaryGenerator: Gen[Binary]     = Gen.oneOf(Zero, One)
  val maxSequenceSize                  = 10

  sealed trait Binary {
    def flip: Binary
  }

  case object One  extends Binary { def flip: Binary = Zero }
  case object Zero extends Binary { def flip: Binary = One  }

  def verifyMachine[S, I, O](name: String, machine: Machine[S, I, O], input: Gen[List[I]])(
    function: List[I] => Boolean
  ): Unit = {

    s"parsing $name" in {
      check(forAll(input) { s =>
        val expected = function(s)
        val actual   = machine.parse(s)
        expected == actual
      })
    }

    s"generating $name" in {
      val validSequences = machine.generate.takeWhile(_.length <= maxSequenceSize).toList

      check(forAll(input.filter(_.size <= maxSequenceSize)) { s =>
        val expected = function(s)
        val actual   = validSequences.contains(s)
        expected == actual
      })
    }
  }

}
