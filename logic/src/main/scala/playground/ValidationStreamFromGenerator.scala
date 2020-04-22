package playground

import monix.eval.Task
import testbot.runtime.{EndpointRequestExecutor, ResponseValidator, ValidatedRequestResponse}
import fs2.Stream

class ValidationStreamFromGenerator(
  requestGenerator: RequestGenerator,
  requestExecutor: EndpointRequestExecutor,
  requestResponseRepository: RequestResponseRepository,
  opportunitiesRepository: OpportunitiesRepository
) {
  def apply(session: Session): Stream[Task, ValidatedRequestResponse] =
    requestStream(session)
      .through(responseStream(session))
      .through(validationStream(session))

  private def requestStream(session: Session): Stream[Task, RequestGeneratorResult] =
    Stream
      .repeatEval(requestGenerator.nextRequest(session))
      .unNoneTerminate

  private def responseStream(session: Session)(requestStream: Stream[Task, RequestGeneratorResult]): Stream[Task, EndpointRequestResponse] =
    requestStream.evalMap(executeRequestAndSaveState(session))

  private def validationStream(session: Session)(responseStream: Stream[Task, EndpointRequestResponse]): Stream[Task, ValidatedRequestResponse] =
    ResponseValidator.stream(session.schema, responseStream)(identity).map(_._2)

  private def executeRequestAndSaveState(session: Session)(request: RequestGeneratorResult): Task[EndpointRequestResponse] =
    for {
      response <- requestExecutor.execute(session.schema, request.nextRequest)
      _        <- requestResponseRepository.saveRequestResponse(session.id, response)
      _        <- opportunitiesRepository.saveOpportunities(session.id, request.opportunities)
    } yield {
      response
    }
}
