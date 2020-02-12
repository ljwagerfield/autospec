package spike.runtime

import cats.data.Chain

case class TestPathWithChecks(id: TestPathId, requests: Chain[EndpointRequestWithChecks])