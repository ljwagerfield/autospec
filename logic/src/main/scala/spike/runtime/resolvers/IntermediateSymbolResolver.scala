package spike.runtime.resolvers

import io.circe.Json
import spike.IntermediateSymbols
import spike.IntermediateSymbols._
import spike.schema.EndpointParameterName
import scala.collection.immutable.{Map => SMap}

object IntermediateSymbolResolver {
  def resolveSymbol(symbol: Symbol, resolvedParams: SMap[EndpointParameterName, Json]): Json =
    BaseSymbolResolver.resolveSymbol(
      BaseSymbolResolver.convertToBaseSymbol(IntermediateSymbols)(symbol)(convertToBaseSymbol(resolvedParams))
    )

  def resolvePredicate(predicate: Predicate, resolvedParams: SMap[EndpointParameterName, Json]): Boolean =
    BaseSymbolResolver.resolvePredicate(
      BaseSymbolResolver.convertToBasePredicate(IntermediateSymbols)(predicate)(convertToBaseSymbol(resolvedParams))
    )

  def convertToBaseSymbol(resolvedParams: SMap[EndpointParameterName, Json])(symbol: OwnSymbols): Json =
    symbol match {
      case Parameter(name) => resolvedParams(name)
    }
}
