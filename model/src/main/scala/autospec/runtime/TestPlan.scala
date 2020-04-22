package autospec.runtime

import cats.data.Chain

case class TestPlan(id: TestPlanId, requests: Chain[EndpointRequestSymbolic])
