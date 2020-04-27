package autospec.runtime

import fs2.Stream
import monix.eval.Task

class ValidatedStreamFromGenerator(requestGenerator: RequestGenerator) {

  def apply(session: Session): Stream[Task, ValidatedRequestResponse] =
    responseStream(session)
      .through(validatedStream(session))

  private def responseStream(session: Session): Stream[Task, EndpointRequestResponse] =
    requestGenerator.stream(session)

  private def validatedStream(
    session: Session
  )(responseStream: Stream[Task, EndpointRequestResponse]): Stream[Task, ValidatedRequestResponse] =
    ResponseValidator.stream(session.schema, responseStream)

}
