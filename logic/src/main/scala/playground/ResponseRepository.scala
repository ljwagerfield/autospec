package playground

import cats.effect.concurrent.Ref
import monix.eval.Task
import spike.schema.EndpointId

class ResponseRepository {
  private val items: Ref[Task, List[EndpointRequestResponse]] = Ref.unsafe[Task, List[EndpointRequestResponse]](Nil)

  def addResponse(response: EndpointRequestResponse): Task[Unit] =
    items.update(response :: _) // Todo: optimisation -- use Queue and drop last item if over 'config.maxHistorySizeForRequestGenerator'.

  def getPreviousResponses(limit: Int): Task[Map[EndpointId, List[EndpointRequestResponse]]] =
    items.get.map(_.take(limit).groupBy(_.request.endpointId))
}
