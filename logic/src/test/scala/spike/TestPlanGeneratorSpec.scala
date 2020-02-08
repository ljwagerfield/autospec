package spike

import spike.RuntimeSymbols.Predicate._
import spike.RuntimeSymbols._

class TestPlanGeneratorSpec extends TestPlanGeneratorSpecBase {
  "TestPlanGenerator.from" should {
    "check postconditions that don't contain references to other endpoints immediately" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        )
      )
    }

    "defer postconditions that contain references to other endpoints" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(1), Literal(42)), // Deferred postcondition from above request.
          Equals(Count(Distinct(ResponseBody(1))), Count(ResponseBody(1)))
        ),
      )
    }

    "accumulate deferred postconditions that contain references to the same endpoint" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        add(52) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(2), Literal(42)),
          Contains(ResponseBody(2), Literal(52)),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
        ),
      )
    }

    "greedily consume deferred postconditions" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(1), Literal(42)), // Consume deferred postcondition.
          Equals(Count(Distinct(ResponseBody(1))), Count(ResponseBody(1)))
        ),
        list() -> checks(
          // Contains(ResponseBody(2), Literal(42)), // Already consumed above.
          Equals(ResponseBody(1), ResponseBody(2)),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
        ),
      )
    }

    "allow identical requests with identical responses" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(1), Literal(42)),
          Equals(Count(Distinct(ResponseBody(1))), Count(ResponseBody(1)))
        ),
        add(42) -> checks(
          Equals(StatusCode(2), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(3), Literal(42)),
          Equals(Count(Distinct(ResponseBody(3))), Count(ResponseBody(3)))
        ),
      )
    }

    "allow identical requests with different responses" in {
      import spike.ListApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        list() -> checks(

        ),
        add(42) -> checks(
          Equals(StatusCode(2), Literal(200))
        ),
        list() -> checks(
          Equals(
            ResponseBody(3),
            Concat(
              ResponseBody(1),
              Literal(42)
            )
          )
        ),
      )
    }

    "use previous endpoint as a 'pre-execution' endpoint" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(1), Literal(42)),
          Equals(Count(Distinct(ResponseBody(1))), Count(ResponseBody(1)))
        ),
        list() -> checks(
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2))),
          Equals(ResponseBody(1), ResponseBody(2))
        )
      )
    }

    "use previous endpoint as a 'pre-execution' endpoint if there have since been mutations, but only if none affect the endpoint" in {
      import spike.SetPairApi.Client._
      test(
        listA() -> checks(
          Equals(Count(Distinct(ResponseBody(0))), Count(ResponseBody(0)))
        ),
        addB(42) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        listA() -> checks(
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2))),
          Equals(ResponseBody(0), ResponseBody(2))
        )
      )
    }

    "not use previous endpoint as a 'pre-execution' endpoint if there have since been known mutations to it" in {
      import spike.SetApi.Client._
      test(
        list() -> checks(
          Equals(Count(Distinct(ResponseBody(0))), Count(ResponseBody(0)))
        ),
        add(42) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(2), Literal(42)),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2))),
          // Equals(ResponseBody(0), ResponseBody(2)) // Discard this check as there's been a mutation since #0 was called
        )
      )
    }
  }
}
