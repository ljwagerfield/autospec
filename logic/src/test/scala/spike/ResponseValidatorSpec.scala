package spike

import spike.RuntimeSymbols.Predicate._
import spike.RuntimeSymbols._

class ResponseValidatorSpec extends ResponseValidatorSpecBase {
  "ResponseValidator" should {
    "immediately check postconditions that don't contain references to other endpoints" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        )
      )
    }

    "support preconditions that refer to other endpoints" in {
      import spike.SetApi.Client._
      test(
        list() -> checks(
          Equals(Count(Distinct(ResponseBody(0))), Count(ResponseBody(0)))
        ),
        removeOrError(42) -> checks(
          Equals(StatusCode(1), Literal(200)),
          Or(Contains(ResponseBody(0),Literal(42)),Equals(StatusCode(1),Literal(404))) // Precondition on previous endpoint
        ),
        list() -> checks(
          Not(Contains(ResponseBody(2), Literal(42))),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
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

    "only consume the last set of deferred postconditions for an endpoint (when calling the same mutation endpoint twice)" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        add(52) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(2), Literal(52)),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
        ),
      )
    }

    "only consume the last set of deferred postconditions for an endpoint (when calling two different mutation endpoints)" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        remove(42) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        list() -> checks(
          Not(Contains(ResponseBody(2), Literal(42))),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
        ),
      )
    }

    // Theoretically we could support this, but felt the complexity outweighed the benefit at the time. Benefit: reduced
    // number of GET requests to validate endpoint postconditions in some situations. Complexity: identifying state
    // boundaries / which requests touch state shared by other requests.
    "only consume the last set of deferred postconditions for an endpoint (when calling two mutation endpoints that touch different state)" in {
      import spike.SetPairApi.Client._
      test(
        addA(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        addB(52) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        listA() -> checks(
          // Contains(ResponseBody(2), Literal(42)), // Reset by request #1
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
        ),
        listB() -> checks(
          Contains(ResponseBody(3), Literal(52)), // Not reset by request #2 because it's pure
          Equals(Count(Distinct(ResponseBody(3))), Count(ResponseBody(3)))
        )
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
            Concat(
              ResponseBody(1),
              Literal(42)
            ),
            ResponseBody(3)
          )
        ),
      )
    }

    "use previous endpoint as a 'reverse lookup' endpoint" in {
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

    "not check preconditions that refer to other endpoints if mutations occur in-between" in {
      import spike.SetApi.Client._
      test(
        list() -> checks(
          Equals(Count(Distinct(ResponseBody(0))), Count(ResponseBody(0)))
        ),
        remove(52) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        removeOrError(42) -> checks(
          Equals(StatusCode(2), Literal(200)),
          // Or(Contains(ResponseBody(0),Literal(42)),Equals(StatusCode(1),Literal(404))) // Invalidated by request #1
        ),
        list() -> checks(
          Not(Contains(ResponseBody(3), Literal(42))),
          Equals(Count(Distinct(ResponseBody(3))), Count(ResponseBody(3)))
        )
      )
    }

    "prevent postconditions referring to earlier endpoints if there have since been related mutations (#1)" in {
      import spike.ListApi.Client._
      test(
        list() -> checks(

        ),
        add(42) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        list() -> checks(
          Equals(
            Concat(
              ResponseBody(0),
              Literal(42)
            ),
            ResponseBody(2)
          ),
          // Equals(ResponseBody(0), ResponseBody(2)) // Discard this check as there's been a mutation since #0 was called
        ),
      )
    }

    "prevent postconditions referring to earlier endpoints if there have since been related mutations (#2)" in {
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

    // Theoretically we could support this, but felt the complexity outweighed the benefit at the time. Benefit: reduced
    // number of GET requests to validate endpoint postconditions in some situations. Complexity: identifying state
    // boundaries / which requests touch state shared by other requests.
    "prevent postconditions referring to earlier endpoints if there have since been unrelated mutations" in {
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
          // Equals(ResponseBody(0), ResponseBody(2)) // Discard this check as there's been a mutation since #0 was called
        )
      )
    }

    "allow endpoints to be explicitly marked as pure" in {
      import spike.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        count() -> checks(

        ),
        list() -> checks(
          Contains(ResponseBody(2), Literal(42)), // Request #1 is explicitly marked as pure, so shouldn't invalidate this postcondition.
          Equals(Count(ResponseBody(2)), ResponseBody(1)),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
        )
      )
    }
  }
}
