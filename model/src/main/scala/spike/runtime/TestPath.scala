package spike.runtime

import cats.data.Chain

case class TestPath(id: TestPathId, requests: Chain[EndpointRequestSymbolic])
