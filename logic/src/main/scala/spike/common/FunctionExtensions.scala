package spike.common

import cats.data.Chain
import cats.{Alternative, Bifoldable, FlatMap, Foldable}
import cats.implicits._

object FunctionExtensions {
  implicit class RichFoo[F[_], A](val fa: F[A]) extends AnyVal {
    def flatPartitionBifold[H[_, _], B, C](
      f: A => H[F[B], F[C]]
    )(implicit A: Alternative[F], F: Foldable[F], F2: FlatMap[F], H: Bifoldable[H]): (F[B], F[C]) = {
      val (b, c) = fa.partitionBifold(f)
      b.flatten -> c.flatten
    }
  }

  implicit class RichChain[A](val value: Chain[A]) extends AnyVal {
    def tail: Chain[A] =
      value.uncons.fold(Chain.empty[A])(_._2)
  }
}
