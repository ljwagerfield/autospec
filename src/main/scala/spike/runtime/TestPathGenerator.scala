package spike.runtime

import spike.schema.ApplicationSchema

class TestPathGenerator {
  def apply(schema: ApplicationSchema): List[TestPath] = {
    // Todo: this is the most complex part of the code. We need to devise meaningful paths through the system.
    // Note: we don't actually calculate any checks here: that's done later. This method is all about generating a
    // _path_ that when executed, is likely to highlight constraint violations. The actual constraint checks are
    // performed elsewhere (in 'TestPathWithChecks' and then in 'EndpointRequestWithChecks').
    // Todo: for now, we can manually devise paths and pass to 'TestPlan(schema, path)', just so that we can play with
    // the constraint checking ability of the application, without having to implement this method.
    // Todo: if an endpoint references another endpoint in either its pre or post conditions (as a 'before' value), then
    // we need to call that endpoint first. If the endpoint references another endpoint in its post condition (as an 'after'),
    // then we need to call that endpoint after. It needs to be called before AND after if both are true.
    ???
  }
}
