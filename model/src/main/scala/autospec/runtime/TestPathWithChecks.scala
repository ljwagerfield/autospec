package autospec.runtime

import cats.data.Chain

case class TestPathWithChecks(id: TestPlanId, requests: Chain[EndpointRequestWithChecks])
