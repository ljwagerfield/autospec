package spike.runtime
import spike.RuntimeSymbols._
import spike.RuntimeSymbols.Predicate._
import cats.implicits._

trait SymbolPrinter {
  def print(symbol: Symbol, currentRequestIndex: Int): String
  def print(request: EndpointRequest, currentRequestIndex: Int): String
}

object ScalaSymbolPrinter extends SymbolPrinter {
  override def print(symbol: Symbol, currentRequestIndex: Int): String = {
    val p = print(_: Symbol, currentRequestIndex)
    symbol match {
      case Literal(value)             => value.toString()
      case LambdaParameter(distance)  => if (distance === 0) "_" else symbol.toString
      case ResponseBody(requestIndex) => if (requestIndex === currentRequestIndex) "body" else s"bodyAt($requestIndex)"
      case StatusCode(requestIndex)   => if (requestIndex === currentRequestIndex) "status" else s"statusAt($requestIndex)"
      case Map(symbol, path)          => s"${p(symbol)}['${path.toList.mkString("']['")}']"
      case FlatMap(symbol, path)      => p(Flatten(Map(symbol, path)))
      case Flatten(symbol)            => s"${p(symbol)}.flatten"
      case Find(symbol, predicate)    => s"${p(symbol)}.find(${p(predicate)})"
      case Count(symbol)              => s"${p(symbol)}.count"
      case Distinct(symbol)           => s"${p(symbol)}.distinct"
      case Prepend(item, collection)  => s"${p(item)} +: ${p(collection)}"
      case Append(collection, item)   => s"${p(collection)} :+ ${p(item)}"
      case Concat(left, right)        => s"${p(left)} ++ ${p(right)}"
      case Equals(left, right)        => wrapParenthesis(s"${p(left)} == ${p(right)}", left, right)
      case And(left, right)           => wrapParenthesis(s"${p(left)} && ${p(right)}", left, right)
      case Or(left, right)            => wrapParenthesis(s"${p(left)} || ${p(right)}", left, right)
      case Not(pred)                  => s"!${p(pred)}"
      case Exists(symbol, predicate)  => s"${p(symbol)}.exists(${p(predicate)})"
      case Contains(collection, item) => s"${p(collection)}.contains(${p(item)})"
    }
  }

  override def print(request: EndpointRequest, currentRequestIndex: Int): String =
    s"${request.endpointId.value}(${request.parameterValues.toList.map(x => s"${x._1.value} = ${print(x._2, currentRequestIndex)}").mkString(", ")})"

  private def wrapParenthesis(value: String, left: Symbol, right: Symbol): String = {
    val leftIsPred =
      left match {
        case _: Predicate => true
        case _            => false
      }
    val rightIsPred =
      right match {
        case _: Predicate => true
        case _            => false
      }

    val shouldWrap = leftIsPred || rightIsPred

    if (shouldWrap)
      s"($value)"
    else
      value
  }
}