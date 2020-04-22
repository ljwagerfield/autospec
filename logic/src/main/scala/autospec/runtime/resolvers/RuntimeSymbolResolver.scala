package autospec.runtime.resolvers

import cats.Id
import cats.data.Chain
import io.circe.Json
import autospec.RuntimeSymbols
import autospec.RuntimeSymbols._
import autospec.runtime.EndpointResponse

object RuntimeSymbolResolver {
  def resolveSymbol(symbol: Symbol, history: Chain[EndpointResponse]): Json =
    BaseSymbolResolver.resolveSymbol(
      BaseSymbolResolver.convertToBaseSymbol[Id, RuntimeSymbols.type](RuntimeSymbols)(symbol)(convertToBaseSymbol(history))
    )

  def resolvePredicate(predicate: Predicate, history: Chain[EndpointResponse]): Boolean =
    BaseSymbolResolver.resolvePredicate(
      BaseSymbolResolver.convertToBasePredicate[Id, RuntimeSymbols.type](RuntimeSymbols)(predicate)(convertToBaseSymbol(history))
    )

  def convertToBaseSymbol(history: Chain[EndpointResponse])(symbol: OwnSymbols): Json = {
    val responseAt = (requestIndex: Int) => history.get(history.size - (requestIndex + 1)).get
    symbol match {
      case StatusCode(requestIndex)   => Json.fromInt(responseAt(requestIndex).status)
      case ResponseBody(requestIndex) => responseAt(requestIndex).body
    }
  }
}
