package autospec.common

import cats.data.{Chain, EitherT, StateT}
import cats.implicits._
import cats.{Alternative, Bifoldable, FlatMap, Foldable, Functor, Monad, Monoid}
import monix.eval.Task

object FunctorExtensions {

  implicit class RichF[F[_], A](val fa: F[A]) extends AnyVal {

    def tap(f: A => Unit)(implicit F: Functor[F]): F[A] =
      fa.map { a =>
        f(a)
        a
      }

    def flatPartitionBifold[H[_, _], B, C](
      f: A => H[F[B], F[C]]
    )(implicit A: Alternative[F], F: Foldable[F], F2: FlatMap[F], H: Bifoldable[H]): (F[B], F[C]) = {
      val (b, c) = fa.partitionBifold(f)
      b.flatten -> c.flatten
    }

    def findAfter(after: A => Boolean, select: A => Boolean)(implicit F: Foldable[F]): Option[A] =
      fa.foldLeft((false: Boolean, None: Option[A])) { (accum, response) =>
        val (isAfter, result) = accum
        if (result.nonEmpty)
          accum
        else if (isAfter)
          if (select(response))
            isAfter -> Some(response)
          else
            accum
        else if (after(response))
          true -> None
        else
          accum
      }._2

    def asRightT[E](implicit F: Functor[F]): EitherT[F, E, A] =
      EitherT.right[E](fa)

  }

  implicit class RichTask[A](val task: Task[A]) extends AnyVal {

    def toEitherT[E]: EitherT[Task, E, A] =
      EitherT.liftF(task)

  }

  implicit class RichList[A](val list: List[A]) extends AnyVal {

    def one: Option[A] =
      list.headOption.filter(_ => list.length === 1)

  }

  implicit class RichChain[A](val value: Chain[A]) extends AnyVal {

    def tail: Chain[A] =
      value.uncons.fold(Chain.empty[A])(_._2)

  }

  implicit class RichOption[A](val value: Option[A]) extends AnyVal {

    def ifNone(thunk: => Unit): Option[A] = {
      thunk
      value
    }

  }

  implicit class RichMapM[K, V, F[_]](val value: Map[K, F[V]]) extends AnyVal {

    def merge(map: Map[K, F[V]])(implicit M: Monoid[F[V]]): Map[K, F[V]] =
      map.foldLeft(value) { (result, kvp) =>
        val (key, value) = kvp
        result.merge(key, value)
      }

    def merge(key: K, values: F[V])(implicit M: Monoid[F[V]]): Map[K, F[V]] = {
      val oldConditions = value.getOrElse(key, M.empty)
      val newConditions = M.combine(oldConditions, values)
      value ++ Map(key -> newConditions)
    }

  }

  implicit class RichMap[K, V](val value: Map[K, V]) extends AnyVal {

    def partitionEither[A, B](callback: V => Either[A, B]): (Map[K, A], Map[K, B]) = {
      val (aList, bList) = value.toList.partitionEither {
        case (k, v) => callback(v).fold(x => Left(k -> x), x => Right(k -> x))
      }
      (
        aList.toMap,
        bList.toMap
      )
    }

    def partitionEitherM[A, B, F[_]: Monad](callback: V => F[Either[A, B]]): F[(Map[K, A], Map[K, B])] =
      value
        .toList
        .partitionEitherM { case (k, v) => callback(v).map(_.fold(x => Left(k -> x), x => Right(k -> x))) }
        .map {
          case (aList, bList) =>
            (
              aList.toMap,
              bList.toMap
            )
        }

    def swap: Map[V, Set[K]] =
      value.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap

  }

  implicit class RichStateT[F[_], S, A](val value: StateT[F, S, A]) extends AnyVal {

    def transformTap[B, SC](f: (S, A) => S)(implicit F: Functor[F]): StateT[F, S, A] =
      value.transform((state, a) => f(state, a) -> a)

  }

}
