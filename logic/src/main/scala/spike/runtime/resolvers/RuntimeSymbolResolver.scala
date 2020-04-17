package spike.runtime.resolvers

import cats.data.Chain
import io.circe.Json
import spike.RuntimeSymbols
import spike.RuntimeSymbols._
import spike.runtime.EndpointResponse

object RuntimeSymbolResolver {
  def resolveSymbol(symbol: Symbol, history: Chain[EndpointResponse]): Json =
    BaseSymbolResolver.resolveSymbol(
      BaseSymbolResolver.convertToBaseSymbol(RuntimeSymbols)(symbol)(convertToBaseSymbol(history))
    )

  def resolvePredicate(predicate: Predicate, history: Chain[EndpointResponse]): Boolean =
    BaseSymbolResolver.resolvePredicate(
      BaseSymbolResolver.convertToBasePredicate(RuntimeSymbols)(predicate)(convertToBaseSymbol(history))
    )

  def convertToBaseSymbol(history: Chain[EndpointResponse])(symbol: OwnSymbols): Json = {
    val responseAt = (requestIndex: Int) => history.get(history.size - (requestIndex + 1)).get
    symbol match {
      case StatusCode(requestIndex)   => Json.fromInt(responseAt(requestIndex).status)
      case ResponseBody(requestIndex) => responseAt(requestIndex).body
    }
  }
}
