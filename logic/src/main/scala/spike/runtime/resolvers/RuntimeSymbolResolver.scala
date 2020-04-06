package spike.runtime.resolvers

import io.circe.Json
import spike.RuntimeSymbols
import spike.RuntimeSymbols._
import spike.runtime.EndpointRequestResponseOld

object RuntimeSymbolResolver {
  def resolveSymbol(symbol: Symbol, history: List[EndpointRequestResponseOld]): Json =
    BaseSymbolResolver.resolveSymbol(
      BaseSymbolResolver.convertToBaseSymbol(RuntimeSymbols)(symbol)(convertToBaseSymbol(history))
    )

  def resolvePredicate(predicate: Predicate, history: List[EndpointRequestResponseOld]): Boolean =
    BaseSymbolResolver.resolvePredicate(
      BaseSymbolResolver.convertToBasePredicate(RuntimeSymbols)(predicate)(convertToBaseSymbol(history))
    )

  def convertToBaseSymbol(history: List[EndpointRequestResponseOld])(symbol: OwnSymbols): Json = {
    val responseAt = (requestIndex: Int) => history(history.size - (requestIndex + 1)).response
    symbol match {
      case StatusCode(requestIndex)   => Json.fromInt(responseAt(requestIndex).status)
      case ResponseBody(requestIndex) => responseAt(requestIndex).body
    }
  }
}
