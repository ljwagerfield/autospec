package autospec.runtime

import autospec.common.ULID
import autospec.schema.ApplicationSchema
import cats.effect.concurrent.Ref
import monix.eval.Task

case class Session(id: SessionId, schema: ApplicationSchema, nextRequestIndex: Ref[Task, EndpointRequestIndex]) {

  def newRequestId(): Task[EndpointRequestId] =
    nextRequestIndex.modify(index => index.increment() -> EndpointRequestId(id, index))

}

object Session {

  def newSession(schema: ApplicationSchema): Task[Session] =
    for {
      sessionId        <- ULID.next[Task].map(SessionId)
      nextRequestIndex <- Ref.of[Task, EndpointRequestIndex](EndpointRequestIndex(0))
    } yield Session(sessionId, schema, nextRequestIndex)

}
