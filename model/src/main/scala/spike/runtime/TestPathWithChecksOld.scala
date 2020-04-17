package spike.runtime

import cats.data.Chain

case class TestPathWithChecksOld(id: TestPathId, requests: Chain[EndpointRequestWithChecksOld])