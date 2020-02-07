package spike.runtime
import spike.RuntimeSymbols._
import cats.implicits._

trait SymbolPrinter {
  def print(symbol: Symbol, currentRequestIndex: Int): String
}

object ScalaSymbolPrinter extends SymbolPrinter {
  override def print(symbol: Symbol, currentRequestIndex: Int): String = {
    val p = print(_: Symbol, currentRequestIndex)
    symbol match {
      case Literal(value)                => value.toString()
      case LambdaParameter(distance)     => if (distance === 0) "_" else symbol.toString
      case ResponseBody(requestIndex)    => if (requestIndex === currentRequestIndex) "body" else s"bodyAt($requestIndex)"
      case StatusCode(requestIndex)      => if (requestIndex === currentRequestIndex) "status" else s"statusAt($requestIndex)"
      case Map(symbol, path)             => s"${p(symbol)}['${path.toList.mkString("']['")}']"
      case Flatten(symbol)               => s"${p(symbol)}.flatten"
      case Find(symbol, predicate)       => s"${p(symbol)}.find(${p(predicate)})"
      case Count(symbol)                 => s"${p(symbol)}.count"
      case Distinct(symbol)              => s"${p(symbol)}.distinct"
      case Predicate.Equals(left, right) => wrapParenthesis(s"${p(left)} == ${p(right)}", left, right)
      case Predicate.And(left, right)    => wrapParenthesis(s"${p(left)} && ${p(right)}", left, right)
      case Predicate.Or(left, right)     => wrapParenthesis(s"${p(left)} || ${p(right)}", left, right)
      case Predicate.Not(pred)           => s"!${p(pred)}"
    }
  }

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