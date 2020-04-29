package autospec.runtime

import autospec.runtime.exceptions.EndpointRequestFailure
import fs2.Stream
import monix.eval.Task

class ValidatedStreamFromGenerator(requestGenerator: RequestGenerator) {

  def apply(session: Session): Stream[Task, Either[EndpointRequestFailure, ValidatedRequestResponse]] =
    responseStream(session)
      .through(validatedStream(session))

  private def responseStream(session: Session): Stream[Task, Either[EndpointRequestFailure, EndpointRequestResponse]] =
    requestGenerator.stream(session)

  private def validatedStream(
    session: Session
  )(
    responseStream: Stream[Task, Either[EndpointRequestFailure, EndpointRequestResponse]]
  ): Stream[Task, Either[EndpointRequestFailure, ValidatedRequestResponse]] =
    ResponseValidator.stream(session.schema, responseStream)(identity, identity).map(_.map(_._2))

}
