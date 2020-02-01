package spike.runtime

import spike.schema.ApplicationSchema

case class TestPathWithChecks(id: TestPathId, requests: List[EndpointRequestWithChecks])

object TestPathWithChecks {
  def apply(schema: ApplicationSchema, testPath: TestPath): TestPathWithChecks = {
    // For each request, we're adding checks for:
    // 1) The current request's pre-conditions
    // 2) The current request's post-conditions that don't involve other endpoints
    // 3) All the previous request's post-conditions that reference the current endpoint.

  }
}