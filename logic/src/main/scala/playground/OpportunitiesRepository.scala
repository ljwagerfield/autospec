package playground

import autospec.runtime.SessionId
import cats.effect.concurrent.Ref
import monix.eval.Task

class OpportunitiesRepository {
  private val sessionsRef: Ref[Task, Map[SessionId, List[Opportunities]]] =
    Ref.unsafe[Task, Map[SessionId, List[Opportunities]]](Map.empty)

  def saveOpportunities(sessionId: SessionId, opportunities: Opportunities): Task[Unit] =
    sessionsRef.update { sessions =>
      val currentSession = sessions.getOrElse(sessionId, Nil)
      val updatedSession = opportunities :: currentSession
      sessions ++ Map(sessionId -> updatedSession)
    }

  def getPreviousOpportunities(sessionId: SessionId, limit: Int): Task[List[Opportunities]] =
    sessionsRef.get.map(_.getOrElse(sessionId, Nil).take(limit))
}
