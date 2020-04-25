package autospec.common

import java.util.concurrent.TimeUnit

import cats.effect.{Clock, Sync}
import cats.kernel.{Eq, Order}
import cats.implicits._
import de.huxhorn.sulky.ulid.ULID.{Value => ULIDValue}
import de.huxhorn.sulky.ulid.{ULID => ULIDGenerator}

case class ULID(value: ULIDValue)

object ULID {

  /**
    * WARNING:
    * ULIDs generated in tight loops may not be causally ordered: the timestamp component has millisecond granularity.
    * For unit tests, use an artificial clock that ticks on every resolve.
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
