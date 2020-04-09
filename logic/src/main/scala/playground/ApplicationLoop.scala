package playground

import monix.eval.Task
import spike.runtime.EndpointRequestExecutor
import cats.implicits._

class ApplicationLoop(
                       requestGenerator: RequestGenerator,
                       requestExecutor: EndpointRequestExecutor,
                       requestResponseRepository: RequestResponseRepository,
                       opportunitiesRepository: OpportunitiesRepository
) {
  def runOnce(session: Session): Task[Option[EndpointRequestResponse]] =
    requestGenerator.nextRequest(session).flatMap(_.traverse { case RequestGeneratorResult(opportunities, request) =>
      for {
        response <- requestExecutor.execute(session.schema, request)
        _        <- requestResponseRepository.saveRequestResponse(session.id, response)
        _        <- opportunitiesRepository.saveOpportunities(session.id, opportunities)
        // Todo: validate response.
        // - We want to re-use the logic from 'TestPlanGenerator' and 'ResponseValidator', but convert into an online/streaming algo.
        // - Also we want to support replaying of certain paths, and the existing code works well for that.
        // - It's a case of integrating them together, not replacing one with the other.
        // Todo: save errors (if any)
        // Todo: save failed test paths
        // Todo: return the error
      } yield {
        response
      }
    })
}
