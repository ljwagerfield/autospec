package spike.runtime.resolvers

import cats.Applicative
import cats.implicits._
import spike.CommonSymbols

object SymbolConverter {
  def convertSymbol[
    F[_]: Applicative, 
    A <: CommonSymbols, 
    B <: CommonSymbols
  ](a: A, b: B)(symbol: a.Symbol)(convert: a.OwnSymbols => F[b.Symbol]): F[b.Symbol] = {
    val convertSym  = convertSymbol(a, b)(_: a.Symbol)(convert)
    val convertPred = convertPredicate(a, b)(_: a.Predicate)(convert)
    symbol match {
      case x: a.Literal               => (b.Literal(x.value): b.Symbol).pure[F]
      case x: a.LambdaParameter       => (b.LambdaParameter(x.distance): b.Symbol).pure[F]
      case x: a.Map                   => (convertSym(x.symbol), convertSym(x.path)).mapN(b.Map(_, _))
      case x: a.FlatMap               => (convertSym(x.symbol), convertSym(x.path)).mapN(b.FlatMap(_, _))
      case x: a.Flatten               => convertSym(x.symbol).map(b.Flatten(_))
      case x: a.Find                  => (convertSym(x.symbol), convertPred(x.predicate)).mapN(b.Find(_, _))
      case x: a.Count                 => convertSym(x.symbol).map(b.Count(_))
      case x: a.Distinct              => convertSym(x.symbol).map(b.Distinct(_))
      case x: a.Prepend               => (convertSym(x.item), convertSym(x.collection)).mapN(b.Prepend(_, _))
      case x: a.Append                => (convertSym(x.collection), convertSym(x.item)).mapN(b.Append(_, _))
      case x: a.Concat                => (convertSym(x.leftCollection), convertSym(x.rightCollection)).mapN(b.Concat(_, _))
      case x: a.Predicate             => convertPred(x).widen[b.Symbol]
      case x: a.OwnSymbols @unchecked => convert(x)
      case x                          => throw new Exception(s"No matches for $x in case statement.")
    }
  }

  def convertPredicate[
    F[_]: Applicative,
    A <: CommonSymbols,
    B <: CommonSymbols
  ](a: A, b: B)(symbol: a.Symbol)(convert: a.OwnSymbols => F[b.Symbol]): F[b.Predicate] = {
    val convertSym  = convertSymbol(a, b)(_: a.Symbol)(convert)
    val convertPred = convertPredicate(a, b)(_: a.Predicate)(convert)
    symbol match {
      case x: a.Predicate.Equals   => (convertSym(x.left), convertSym(x.right)).mapN(b.Predicate.Equals(_, _))
      case x: a.Predicate.And      => (convertPred(x.left), convertPred(x.right)).mapN(b.Predicate.And(_, _))
      case x: a.Predicate.Or       => (convertPred(x.left), convertPred(x.right)).mapN(b.Predicate.Or(_, _))
      case x: a.Predicate.Not      => convertPred(x.predicate).map(b.Predicate.Not(_))
      case x: a.Predicate.Exists   => (convertSym(x.symbol), convertPred(x.predicate)).mapN(b.Predicate.Exists(_, _))
      case x: a.Predicate.Contains => (convertSym(x.collection), convertSym(x.item)).mapN(b.Predicate.Contains(_, _))
      case x                       => throw new Exception(s"No matches for $x in case statement.")
    }
  }
}
