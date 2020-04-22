package autospec.runtime.resolvers

import cats.Id
import io.circe.Json
import autospec.IntermediateSymbols
import autospec.IntermediateSymbols._
import autospec.schema.EndpointParameterName

import scala.collection.immutable.{Map => SMap}

object IntermediateSymbolResolver {
  def resolveSymbol(symbol: Symbol, resolvedParams: SMap[EndpointParameterName, Json]): Json =
    BaseSymbolResolver.resolveSymbol(
      BaseSymbolResolver.convertToBaseSymbol[Id, IntermediateSymbols.type](IntermediateSymbols)(symbol)(convertToBaseSymbol(resolvedParams))
    )

  def resolvePredicate(predicate: Predicate, resolvedParams: SMap[EndpointParameterName, Json]): Boolean =
    BaseSymbolResolver.resolvePredicate(
      BaseSymbolResolver.convertToBasePredicate[Id, IntermediateSymbols.type](IntermediateSymbols)(predicate)(convertToBaseSymbol(resolvedParams))
    )

  def convertToBaseSymbol(resolvedParams: SMap[EndpointParameterName, Json])(symbol: OwnSymbols): Json =
    symbol match {
      case Parameter(name) => resolvedParams(name)
    }
}
