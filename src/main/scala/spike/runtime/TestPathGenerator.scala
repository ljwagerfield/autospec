package spike.runtime

import spike.schema.ApplicationSchema

class TestPathGenerator {
  def apply(schema: ApplicationSchema): List[TestPath] = {
    // Todo: if an endpoint references another endpoint in either its pre or post conditions (as a 'before' value), then
    // we need to call that endpoint first. If the endpoint references another endpoint in its post condition (as an 'after'),
    // then we need to call that endpoint after. It needs to be called before AND after if both are true.
  }
}
