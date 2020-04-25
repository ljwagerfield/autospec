package autospec.runtime

import cats.implicits._
import autospec.RuntimeSymbolsIndexed.{ResponseBody, StatusCode}
import autospec.{CommonSymbols, RuntimeSymbolsIndexed => R, SchemaSymbols => S}

trait SymbolPrinter {
  def print(symbol: S.Symbol): String
  def print(symbol: R.Symbol, currentRequest: EndpointRequestIndex): String
  def print(request: EndpointRequestSymbolic, currentRequest: EndpointRequestIndex): String
}

object ScalaSymbolPrinter extends SymbolPrinter {
  override def print(symbol: S.Symbol): String = {
    val p = print(_: S.Symbol)

    common(S)(symbol) {
      case S.ResponseBody                                 => "body"
      case S.StatusCode                                   => "status"
      case S.Parameter(name)                              => name.value
      case S.Endpoint(id, params, evaluateAfterExecution) =>
        val prePost      = if (evaluateAfterExecution) "post" else "pre"
        val methodName   = id.value
        val paramsString = params
          .map { case (name, value) => s"$name=${p(value)}" }
          .mkString(", ")

        s"$prePost.$methodName($paramsString)"
    }
  }

  override def print(symbol: R.Symbol, currentRequest: EndpointRequestIndex): String =
    common(R)(symbol) {
      case ResponseBody(requestIndex) => if (requestIndex === currentRequest) "body" else s"bodyAt($requestIndex)"
      case StatusCode(requestIndex)   => if (requestIndex === currentRequest) "status" else s"statusAt($requestIndex)"
    }

  private def common(family: CommonSymbols)(symbol: family.Symbol)(specialized: family.OwnSymbols => String): String = {
    val p = common(family)(_: family.Symbol)(specialized)
    symbol match {
      case x: family.Literal            => x.value.toString()
      case x: family.LambdaParameter    => if (x.distance === 0) "_" else symbol.toString
      case x: family.Map                => s"${p(x.symbol)}[${{p(x.path)}}]"
      case x: family.FlatMap            => p(family.Flatten(family.Map(x.symbol, x.path)))
      case x: family.Flatten            => s"${p(x.symbol)}.flatten"
      case x: family.Find               => s"${p(x.symbol)}.find(${p(x.predicate)})"
      case x: family.Count              => s"${p(x.symbol)}.count"
      case x: family.Distinct           => s"${p(x.symbol)}.distinct"
      case x: family.Add                => s"${p(x.left)} + ${p(x.right)}"
      case x: family.Subtract           => s"${p(x.left)} - ${p(x.right)}"
      case x: family.Multiply           => s"${p(x.left)} * ${p(x.right)}"
      case x: family.Divide             => s"${p(x.left)} / ${p(x.right)}"
      case x: family.Concat             => s"${p(x.leftCollection)} ++ ${p(x.rightCollection)}"
      case x: family.Predicate.Equals   => wrapParenthesis(family)(s"${p(x.left)} == ${p(x.right)}", x.left, x.right)
      case x: family.Predicate.And      => wrapParenthesis(family)(s"${p(x.left)} && ${p(x.right)}", x.left, x.right)
      case x: family.Predicate.Or       => wrapParenthesis(family)(s"${p(x.left)} || ${p(x.right)}", x.left, x.right)
      case x: family.Predicate.Not      => s"!${p(x.predicate)}"
      case x: family.Predicate.Exists   => s"${p(x.symbol)}.exists(${p(x.predicate)})"
      case x: family.Predicate.Contains => s"${p(x.collection)}.contains(${p(x.item)})"
      case x                            => specialized(x.asInstanceOf[family.OwnSymbols])
    }
  }

  override def print(request: EndpointRequestSymbolic, currentRequest: EndpointRequestIndex): String =
    s"${request.endpointId.value}(${request.parameterValues.toList.map(x => s"${x._1.value} = ${print(x._2, currentRequest)}").mkString(", ")})"

  private def wrapParenthesis(family: CommonSymbols)(value: String, left: family.Symbol, right: family.Symbol): String = {
    val leftIsPred =
      left match {
        case _: family.Predicate => true
        case _                   => false
      }
    val rightIsPred =
      right match {
        case _: family.Predicate => true
        case _                   => false
      }

    val shouldWrap = leftIsPred || rightIsPred

    if (shouldWrap)
      s"($value)"
    else
      value
  }
}
