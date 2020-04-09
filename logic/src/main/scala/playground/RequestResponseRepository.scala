package playground

import cats.effect.concurrent.Ref
import monix.eval.Task
import spike.schema.EndpointId

class RequestResponseRepository {
  private val sessionsRef: Ref[Task, Map[SessionId, List[EndpointRequestResponse]]] = Ref.unsafe[Task, Map[SessionId, List[EndpointRequestResponse]]](Map.empty)

  def saveRequestResponse(sessionId: SessionId, response: EndpointRequestResponse): Task[Unit] =
    sessionsRef.update { sessions =>
      val currentSession = sessions.getOrElse(sessionId, Nil)
      val updatedSession = response :: currentSession
      sessions ++ Map(sessionId -> updatedSession)
    }

  def getPreviousResponses(sessionId: SessionId, limit: Int): Task[Map[EndpointId, List[EndpointRequestResponse]]] =
    sessionsRef.get.map(_.getOrElse(sessionId, Nil).take(limit).groupBy(_.request.endpointId))
}
