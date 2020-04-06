package playground

import monix.eval.Task
import spike.runtime.EndpointRequestExecutor
import spike.schema.ApplicationSchema
import cats.implicits._

class ApplicationLoop(
  requestGenerator: RequestGenerator,
  requestExecutor: EndpointRequestExecutor,
  responseRepository: ResponseRepository,
  opportunityRepository: OpportunitiesRepository
) {
  def runOnce(schema: ApplicationSchema): Task[Option[EndpointRequestResponse]] =
    requestGenerator.nextRequest(schema).flatMap(_.traverse { case RequestGeneratorResult(opportunities, request) =>
      for {
        response <- requestExecutor.execute(schema, request)
        _        <- responseRepository.addResponse(response)
        _        <- opportunityRepository.addOpportunities(opportunities)
        // Todo: validate response
        // Todo: save errors (if any)
        // Todo: return the error
      } yield {
        response
      }
    })
}
