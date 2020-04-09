package spike.common

import cats.kernel.Eq
import de.huxhorn.sulky.ulid.ULID.{Value => ULIDValue}

case class ULID(value: ULIDValue)

object ULID {
  implicit val eq: Eq[ULID] = Eq.fromUniversalEquals
}