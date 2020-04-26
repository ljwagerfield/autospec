package autospec.runtime.resolvers

import autospec.RuntimeSymbolsLike
import autospec.{RuntimeSymbolsExecuted => RE}
import autospec.{RuntimeSymbolsIndexed => RI}
import cats.Id
import cats.data.Chain
import io.circe.Json
import autospec.runtime.{EndpointRequestId, EndpointRequestIndex, EndpointResponse}

object RuntimeSymbolResolver {

  private val resolveByIndex =
    (history: Chain[EndpointResponse]) => (idx: EndpointRequestIndex) => history.get(idx.index).get

  def resolveSymbol(symbol: RI.Symbol, history: Chain[EndpointResponse]): Json =
    resolveSymbol[RI.type, EndpointRequestIndex](RI)(symbol, convertToBaseSymbolRI(resolveByIndex(history)))

  def resolvePredicate(symbol: RI.Predicate, history: Chain[EndpointResponse]): Boolean =
    resolvePredicate[RI.type, EndpointRequestIndex](RI)(symbol, convertToBaseSymbolRI(resolveByIndex(history)))

  def resolveSymbol(symbol: RE.Symbol, resolve: EndpointRequestId => EndpointResponse): Json =
    resolveSymbol[RE.type, EndpointRequestId](RE)(symbol, convertToBaseSymbolRE(resolve))

  def resolvePredicate(symbol: RE.Predicate, resolve: EndpointRequestId => EndpointResponse): Boolean =
    resolvePredicate[RE.type, EndpointRequestId](RE)(symbol, convertToBaseSymbolRE(resolve))

  private def resolveSymbol[A <: RuntimeSymbolsLike[B], B](
    family: A
  )(symbol: family.Symbol, resolve: family.OwnSymbols => Json): Json =
    BaseSymbolResolver.resolveSymbol(
      BaseSymbolResolver.convertToBaseSymbol[Id, A](family)(symbol)(resolve)
    )

  private def resolvePredicate[A <: RuntimeSymbolsLike[B], B](
    family: A
  )(predicate: family.Predicate, resolve: family.OwnSymbols => Json): Boolean =
    BaseSymbolResolver.resolvePredicate(
      BaseSymbolResolver.convertToBasePredicate[Id, A](family)(predicate)(resolve)
    )

  private def convertToBaseSymbolRE(resolve: EndpointRequestId => EndpointResponse)(symbol: RE.OwnSymbols): Json =
    symbol match {
      case RE.StatusCode(requestIndex)   => Json.fromInt(resolve(requestIndex).status)
      case RE.ResponseBody(requestIndex) => resolve(requestIndex).body
    }

  private def convertToBaseSymbolRI(resolve: EndpointRequestIndex => EndpointResponse)(symbol: RI.OwnSymbols): Json =
    symbol match {
      case RI.StatusCode(requestIndex)   => Json.fromInt(resolve(requestIndex).status)
      case RI.ResponseBody(requestIndex) => resolve(requestIndex).body
    }

}
