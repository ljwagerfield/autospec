package playground

import spike.schema.EndpointId

case class ApplicationState(requestsByEndpoint: Map[EndpointId, List[EndpointRequestResponse]]) {
  def add(response: EndpointRequestResponse): ApplicationState = {
    val endpointId   = response.request.endpointId
    val oldResponses = requestsByEndpoint.getOrElse(endpointId, Nil)
    val newResponses = response :: oldResponses
    ApplicationState(
      requestsByEndpoint + (endpointId -> newResponses)
    )
  }
}

object ApplicationState {
  val empty = ApplicationState(Map.empty)
}
