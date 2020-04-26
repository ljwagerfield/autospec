package autospec.common

import java.util.UUID
import java.util.concurrent.TimeUnit

import cats.effect.{Clock, Sync}
import cats.kernel.{Eq, Order}
import cats.implicits._
import de.huxhorn.sulky.ulid.ULID.{Value => ULIDValue}
import de.huxhorn.sulky.ulid.{ULID => ULIDGenerator}

case class ULID(value: ULIDValue) {
  def toUUID: UUID = new UUID(value.getMostSignificantBits, value.getLeastSignificantBits)

  override def toString: String = toUUID.toString
}

object ULID {

  /**
    * WARNING:
    * - NOT CAUSALLY ORDERED!
    * - Do not use this to order items by the order in which they happened.
    * - The timestamp component has millisecond granularity, so ULIDs issued in tight loops on the same machine will be
    *   arrive out-of-order. Further, ULIDs issued across separate machines will likely be milliseconds apart.
    * - The only reason we use ULIDs instead of UUIDs is to reduce index fragmentation in SQL databases.
    */
  implicit val ordering: Ordering[ULID] = scala.Ordering.by(_.value)
  implicit val order: Order[ULID]       = Order.fromOrdering

  implicit val eq: Eq[ULID] = Eq.fromUniversalEquals

  private val ulidGenerator = new ULIDGenerator

  def next[F[_]: Sync: Clock]: F[ULID] =
    for {
      millis <- implicitly[Clock[F]].realTime(TimeUnit.MILLISECONDS)
      ulid   <- Sync[F].delay(ULID(ulidGenerator.nextValue(millis)))
    } yield ulid

}
