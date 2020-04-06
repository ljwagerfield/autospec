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

  implicit class RichMap[K, V](val value: Map[K, V]) extends AnyVal {
    def partitionEither[A, B](callback: V => Either[A, B]): (Map[K, A], Map[K, B]) = {
      val (aList, bList) = value.toList.partitionEither { case (k, v) => callback(v).fold(x => Left(k -> x), x => Right(k -> x)) }
      (
        aList.toMap,
        bList.toMap
      )
    }

    def swap: Map[V, List[K]] =
      value.groupMap(_._2)(_._1).view.mapValues(_.toList).toMap
  }
}
