package spike.common

import cats.data.{Chain, EitherT, NonEmptyList}
import cats.{Alternative, Bifoldable, FlatMap, Foldable}
import cats.implicits._
import monix.eval.Task

object FunctorExtensions {
  implicit class RichF[F[_], A](val fa: F[A]) extends AnyVal {
    def flatPartitionBifold[H[_, _], B, C](
      f: A => H[F[B], F[C]]
    )(implicit A: Alternative[F], F: Foldable[F], F2: FlatMap[F], H: Bifoldable[H]): (F[B], F[C]) = {
      val (b, c) = fa.partitionBifold(f)
      b.flatten -> c.flatten
    }
  }

  implicit class RichTask[A](val task: Task[A]) extends AnyVal {
    def toEitherT[E]: EitherT[Task, E, A] =
      EitherT.liftF(task)
  }

  implicit class RichList[A](val list: List[A]) extends AnyVal {
    def toNEL: Option[NonEmptyList[A]] =
      NonEmptyList.fromList(list)
  }

  implicit class RichChain[A](val value: Chain[A]) extends AnyVal {
    def tail: Chain[A] =
      value.uncons.fold(Chain.empty[A])(_._2)
  }
}
