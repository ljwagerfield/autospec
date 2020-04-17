package spike.runtime

import cats.data.Chain

case class TestPathOld(id: TestPathId, requests: Chain[EndpointRequestSymbolic])
