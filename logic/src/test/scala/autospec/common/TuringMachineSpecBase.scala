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
        val actual   = machine.parse(s)._2
        println(expected.toString + " = " + s.toString())
        expected == actual
      })
    }

    s"generating $name" in {
      val sequences              = machine.generate.take(100)
      val nonEmptySequenceExists = sequences.exists(_.nonEmpty)
      val allSequencesValid      = sequences.forall(machine.parse(_)._2)
      nonEmptySequenceExists shouldBe true
      allSequencesValid shouldBe true
    }
  }

}
