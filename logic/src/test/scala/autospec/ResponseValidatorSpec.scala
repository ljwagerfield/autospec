package autospec

import autospec.ResponseValidatorSpecBase.TestInstruction.{RunAssertion, SimulateRequestFailure}
import autospec.RuntimeSymbolsIndexed.Predicate._
import autospec.RuntimeSymbolsIndexed._
import autospec.LocalSchemaSymbols.{Predicate => SP}
import autospec.runtime.EndpointRequestSymbolic
import autospec.schema.{EndpointDefinition, EndpointId, EndpointParameterName}
import autospec.{LocalSchemaSymbols => S}

import scala.collection.immutable.{Map => SMap}

class ResponseValidatorSpec extends ResponseValidatorSpecBase {
  "ResponseValidator" should {
    "immediately check postconditions that don't contain references to other endpoints" in {
      import autospec.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        )
      )
    }

    "support preconditions that refer to other endpoints" in {
      import autospec.SetApi.Client._
      test(
        list() -> checks(
          Equals(Count(Distinct(ResponseBody(0))), Count(ResponseBody(0)))
        ),
        removeOrError(42) -> checks(
          Equals(StatusCode(1), Literal(200)),
          Or(
            Contains(ResponseBody(0), Literal(42)),
            Equals(StatusCode(1), Literal(404))
          ) // Precondition on previous endpoint
        ),
        list() -> checks(
          Not(Contains(ResponseBody(2), Literal(42))),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
        )
      )
    }

    "defer postconditions that contain references to other endpoints" in {
      import autospec.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(1), Literal(42)), // Deferred postcondition from above request.
          Equals(Count(Distinct(ResponseBody(1))), Count(ResponseBody(1)))
        )
      )
    }

    "only consume the last set of deferred postconditions for an endpoint (when calling the same mutation endpoint twice)" in {
      import autospec.SetApi.Client._
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
        )
      )
    }

    "only consume the last set of deferred postconditions for an endpoint (when calling two different mutation endpoints)" in {
      import autospec.SetApi.Client._
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
        )
      )
    }

    // Theoretically we could support this, but felt the complexity outweighed the benefit at the time. Benefit: reduced
    // number of GET requests to validate endpoint postconditions in some situations. Complexity: identifying state
    // boundaries / which requests touch state shared by other requests.
    "only consume the last set of deferred postconditions for an endpoint (when calling two mutation endpoints that touch different state)" in {
      import autospec.SetPairApi.Client._
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
      import autospec.SetApi.Client._
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
        )
      )
    }

    "allow identical requests with identical responses" in {
      import autospec.SetApi.Client._
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
        )
      )
    }

    "allow identical requests with different responses" in {
      import autospec.ListApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        list() -> checks(
          ),
        add(52) -> checks(
          Equals(StatusCode(2), Literal(200))
        ),
        list() -> checks(
          Equals(
            Concat(
              ResponseBody(1),
              Literal(52)
            ),
            ResponseBody(3)
          )
        )
      )
    }

    "use previous endpoint as a 'reverse lookup' endpoint" in {
      import autospec.SetApi.Client._
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
      import autospec.SetApi.Client._
      test(
        list() -> checks(
          Equals(Count(Distinct(ResponseBody(0))), Count(ResponseBody(0)))
        ),
        remove(52) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        removeOrError(42) -> checks(
          Equals(StatusCode(2), Literal(200))
          // Or(Contains(ResponseBody(0),Literal(42)),Equals(StatusCode(1),Literal(404))) // Invalidated by request #1
        ),
        list() -> checks(
          Not(Contains(ResponseBody(3), Literal(42))),
          Equals(Count(Distinct(ResponseBody(3))), Count(ResponseBody(3)))
        )
      )
    }

    "prevent postconditions referring to earlier endpoints if there have since been related mutations" in {
      import autospec.SetApi.Client._
      test(
        list() -> checks(
          Equals(Count(Distinct(ResponseBody(0))), Count(ResponseBody(0)))
        ),
        add(42) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        list() -> checks(
          Contains(ResponseBody(2), Literal(42)),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
          // Equals(ResponseBody(0), ResponseBody(2)) // Discard this check as there's been a mutation since #0 was called
        )
      )
    }

    "prevent postconditions referring to earlier endpoints if there has since been a mutation, even if said earlier endpoint is kept in-scope by a condition deferred by the mutation" in {
      import autospec.ListApi.Client._
      test(
        list() -> checks(
          ),
        // IMPORTANT FOR THIS TEST: 'add' defers a condition that refers to both 0 and 2, but 2 cannot see 0 for its
        // own postconditions, as 1 mutates in between.
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
          )
          // Equals(ResponseBody(0), ResponseBody(2)) // Discard this check as there's been a mutation since #0 was called
        )
      )
    }

    // Theoretically we could support this, but felt the complexity outweighed the benefit at the time. Benefit: reduced
    // number of GET requests to validate endpoint postconditions in some situations. Complexity: identifying state
    // boundaries / which requests touch state shared by other requests.
    "prevent postconditions referring to earlier endpoints if there have since been unrelated mutations" in {
      import autospec.SetPairApi.Client._
      test(
        listA() -> checks(
          Equals(Count(Distinct(ResponseBody(0))), Count(ResponseBody(0)))
        ),
        addB(42) -> checks(
          Equals(StatusCode(1), Literal(200))
        ),
        listA() -> checks(
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
          // Equals(ResponseBody(0), ResponseBody(2)) // Discard this check as there's been a mutation since #0 was called
        )
      )
    }

    "allow endpoints to be explicitly marked as pure" in {
      import autospec.SetApi.Client._
      test(
        add(42) -> checks(
          Equals(StatusCode(0), Literal(200))
        ),
        count() -> checks(
          ),
        list() -> checks(
          Contains(
            ResponseBody(2),
            Literal(42)
          ), // Request #1 is explicitly marked as pure, so shouldn't invalidate this postcondition.
          Equals(Count(ResponseBody(2)), ResponseBody(1)),
          Equals(Count(Distinct(ResponseBody(2))), Count(ResponseBody(2)))
        )
      )
    }

    "support forward lookups appearing BEFORE reverse lookups within the same condition for a mutating endpoint" in {
      testInlineSpec(
        EndpointDefinition(
          EndpointId("list"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          Nil
        ),
        EndpointDefinition(
          EndpointId("delete"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          List(
            SP.Equals(
              S.Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true),
              S.Subtract(
                S.Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = false),
                S.Parameter("value")
              )
            )
          )
        )
      )(
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> checks(
          ),
        EndpointRequestSymbolic(EndpointId("delete"), SMap(EndpointParameterName("value") -> Literal(42))) -> checks(
          ),
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> checks(
          Equals(ResponseBody(2), Subtract(ResponseBody(0), Literal(42)))
        )
      )
    }

    "support forward lookups appearing BEFORE nested reverse lookups within the same condition for a mutating endpoint" in {
      testInlineSpec(
        EndpointDefinition(
          EndpointId("elementAt"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          Nil
        ),
        EndpointDefinition(
          EndpointId("count"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          Nil
        ),
        EndpointDefinition(
          EndpointId("append"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          List(
            SP.Equals(
              S.Endpoint(
                EndpointId("elementAt"),
                SMap(
                  EndpointParameterName("value") -> S.Subtract(
                    S.Endpoint(EndpointId("count"), SMap.empty, evaluateAfterExecution = true),
                    S.Literal(2)
                  )
                ),
                evaluateAfterExecution = true
              ),
              S.Endpoint(
                EndpointId("elementAt"),
                SMap(
                  EndpointParameterName("value") -> S.Subtract(
                    S.Endpoint(EndpointId("count"), SMap.empty, evaluateAfterExecution = false),
                    S.Literal(1)
                  )
                ),
                evaluateAfterExecution = false
              )
            )
          )
        )
      )(
        EndpointRequestSymbolic(EndpointId("append"), SMap(EndpointParameterName("value") -> Literal(42))) -> checks(
          ),
        EndpointRequestSymbolic(EndpointId("count"), SMap.empty) -> checks(
          ),
        EndpointRequestSymbolic(EndpointId("elementAt"), SMap(EndpointParameterName("value") -> Literal(-1))) -> checks(
          ),
        EndpointRequestSymbolic(EndpointId("append"), SMap(EndpointParameterName("value") -> Literal(50))) -> checks(
          ),
        EndpointRequestSymbolic(EndpointId("count"), SMap.empty) -> checks(
          ),
        EndpointRequestSymbolic(EndpointId("elementAt"), SMap(EndpointParameterName("value") -> Literal(-2))) -> checks(
          Equals(ResponseBody(5), ResponseBody(2))
        )
      )
    }

    // A meta test (to test the test framework itself): ensures every request in the test paths are executed by the test
    // framework and their conditions checked, even if there's a failing request in the middle of the path.
    "ensures tests continue after failures" in {
      var wasCalled = false

      testInlineSpec(
        EndpointDefinition(
          EndpointId("list"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          Nil
        )
      )(
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> checks(),
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> SimulateRequestFailure,
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> RunAssertion(_ => wasCalled = true)
      )

      wasCalled shouldBe true
    }

    "prevent postconditions being deferred across failed requests for mutating endpoints" in {
      testInlineSpec(
        EndpointDefinition(
          EndpointId("list"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          List(
            SP.Equals(
              S.Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true),
              S.ResponseBody
            )
          ),
          forcePure = true
        ),
        EndpointDefinition(
          EndpointId("clear"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          List(
            SP.Equals(
              S.Count(
                S.Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true) // FORWARD LOOKUP
              ),
              S.Literal(0)
            )
          )
        )
      )(
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> checks(
          ),
        EndpointRequestSymbolic(EndpointId("clear"), SMap.empty) -> SimulateRequestFailure,
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> checks(
          // No checks, as failing mutating request #1 clears deferred postconditions from request #0.
        ),
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> checks(
          Equals(ResponseBody(3), ResponseBody(2))
        )
      )
    }

    "prevent postconditions referring to requests that occurred before a failed request for a mutating endpoint" in {
      testInlineSpec(
        EndpointDefinition(
          EndpointId("list"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          List(
            SP.Equals(
              S.Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = false), // REVERSE LOOKUP
              S.ResponseBody
            )
          ),
          forcePure = true
        ),
        EndpointDefinition(
          EndpointId("clear"),
          apiId,
          method,
          path,
          Nil,
          Nil,
          List(
            SP.Equals(
              S.Count(
                S.Endpoint(EndpointId("list"), SMap.empty, evaluateAfterExecution = true)
              ),
              S.Literal(0)
            )
          )
        )
      )(
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> checks(
          ),
        EndpointRequestSymbolic(EndpointId("clear"), SMap.empty) -> SimulateRequestFailure,
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> checks(
          // No checks, as failing mutating request #1 sets new boundary that forbids reverse lookups to #0.
        ),
        EndpointRequestSymbolic(EndpointId("list"), SMap.empty) -> checks(
          Equals(ResponseBody(2), ResponseBody(3))
        )
      )
    }

  }
}
