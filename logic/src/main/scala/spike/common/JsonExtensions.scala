package spike.common

import io.circe.{Json, JsonNumber}

object JsonExtensions {
  implicit class RichJson(val json: Json) extends AnyVal {
    /**
     * Handle 'null' in the same way as JavaScript does (implicitly convert to '0').
     */
    def asNumberNullAsZero: Option[JsonNumber] =
      if (json.isNull)
        Json.fromInt(0).asNumber
      else
        json.asNumber
  }
  implicit class RichJsonNumber(val number: JsonNumber) extends AnyVal {
    def +(other: JsonNumber): JsonNumber =
      binaryReduce(other, _ + _)

    def -(other: JsonNumber): JsonNumber =
      binaryReduce(other, _ - _)

    def *(other: JsonNumber): JsonNumber =
      binaryReduce(other, _ * _)

    def /(other: JsonNumber): JsonNumber =
      binaryReduce(other, _ / _)

    private def binaryReduce(other: JsonNumber, op: (BigDecimal, BigDecimal) => BigDecimal): JsonNumber =
      Json.fromBigDecimal(
        op(number.toBigDecimal.get, other.toBigDecimal.get)
      ).asNumber.get
  }
}
