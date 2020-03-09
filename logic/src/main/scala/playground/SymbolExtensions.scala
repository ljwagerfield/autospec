package playground

import spike.{SchemaSymbols => S}
import spike.SchemaSymbols.{Predicate => SP}

import spike.{ResolvedSymbols => R}
import spike.ResolvedSymbols.{Predicate => RP}

object SymbolExtensions {
  implicit class RichSchemaSymbol(val symbol: S.Symbol) extends AnyVal {
    def toList: List[S.Symbol] = {
      symbol match {
        case x: S.Literal                     => List(x)
        case x: S.LambdaParameter             => List(x)
        case x: S.Parameter                   => List(x)
        case x @ S.ResponseBody               => List(x)
        case x @ S.StatusCode                 => List(x)
        case x: S.Endpoint                    => x :: x.parameters.values.toList.flatMap(_.toList)
        case x @ S.Map(symbol, _)             => x :: symbol.toList
        case x @ S.FlatMap(symbol, _)         => x :: symbol.toList
        case x @ S.Flatten(symbol)            => x :: symbol.toList
        case x @ S.Find(symbol, predicate)    => x :: symbol.toList ::: predicate.toList
        case x @ S.Count(symbol)              => x :: symbol.toList
        case x @ S.Distinct(symbol)           => x :: symbol.toList
        case x @ S.Prepend(item, collection)  => x :: item.toList ::: collection.toList
        case x @ S.Append(collection, item)   => x :: item.toList ::: collection.toList
        case x @ S.Concat(left, right)        => x :: right.toList ::: left.toList
        case x @ SP.Equals(left, right)        => x :: right.toList ::: left.toList
        case x @ SP.And(left, right)           => x :: right.toList ::: left.toList
        case x @ SP.Or(left, right)            => x :: right.toList ::: left.toList
        case x @ SP.Not(predicate)             => x :: predicate.toList
        case x @ SP.Exists(symbol, predicate)  => x :: predicate.toList ::: symbol.toList
        case x @ SP.Contains(collection, item) => x :: item.toList ::: collection.toList
      }
    }
  }
}
