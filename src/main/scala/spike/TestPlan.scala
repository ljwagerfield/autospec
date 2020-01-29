package spike

import spike.schema.ApplicationSchema
import spike.runtime.TestPath

/**
 * GetActualResult(previous: List[(EndpointCall, Result)], current: EndpointCall): Result
 *
 * GetExpectedResult(schema)(previous: List[(EndpointCall, Result)], current: EndpointCall): Result
 *
 * Then... we just see if they match!
 *
 * Note: in 'GetExpectedResult' we need to take into account the post-conditions of calls in the 'previous'
 * list too.
 */

//case class TestPlan()
//
//object TestPlan {
//  /**
//   * Function is responsible for devising the assertions to run after each endpoint call.
//   */
//  def apply(schema: ApplicationSchema, path: TestPath): TestPlan = {
//
//  }
//}