package spike.runtime

import cats.implicits._
import cats.data.NonEmptyList
import spike.runtime.resolvers.RuntimeSymbolResolver
import spike.schema.ConditionId

object ResponseValidator {
  def validateResponse(history: List[EndpointRequestResponseOld], request: EndpointRequestWithChecks, response: EndpointResponse): Either[NonEmptyList[ConditionId], EndpointRequestResponseOld] = {
    val current     = EndpointRequestResponseOld(request.request, response)
    val fullHistory = current :: history
    request
      .checks
      .view
      .mapValues(RuntimeSymbolResolver.resolvePredicate(_, fullHistory))
      .toList
      .collect { case (conditionId, satisfied) if !satisfied => conditionId}
      .toNel
      .toLeft(current)
  }
}
