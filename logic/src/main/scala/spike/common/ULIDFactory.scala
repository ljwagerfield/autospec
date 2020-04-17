package spike.common

import java.util.concurrent.TimeUnit

import cats.effect.{Clock, Sync}
import de.huxhorn.sulky.ulid.{ULID => ULIDInternalFactory}
import cats.implicits._

class ULIDFactory {
  private val internalFactory = new ULIDInternalFactory

  def nextULID[F[_]: Sync: Clock]: F[ULID] =
    for {
      millis <- implicitly[Clock[F]].realTime(TimeUnit.MILLISECONDS)
      ulid   <- Sync[F].delay(ULID(internalFactory.nextValue(millis)))
    } yield {
      ulid
    }
}
