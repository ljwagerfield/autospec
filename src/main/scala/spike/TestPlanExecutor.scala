package spike

import spike.schema.ApplicationSchema
import spike.runtime.TestPath

object TestPlanExecutor {
  def execute(schema: ApplicationSchema, plan: TestPath): ??? = {
    // Maybe we a 'TestPath' and a 'TestPan': TestPath is just what TestPlan is now, but then TestPlan actually dictates
    // 'this needs to be that' and so on.
    // The idea is that there should be NO logic in the test plan executor.


    // Todo: if an endpoint references another endpoint in either its pre or post conditions (as a 'before' value), then
    // we need to call that endpoint first. If the endpoint references another endpoint in its post condition (as an 'after'),
    // then we need to call that endpoint after. It needs to be called before AND after if both are true.
  }
}
