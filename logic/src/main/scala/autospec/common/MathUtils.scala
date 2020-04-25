package autospec.common

import scala.util.Random
import cats.implicits._
import alleycats.std.all._

object MathUtils {

  def weightedRandom[A](items: List[A])(weight: A => Int): Option[A] = {
    var totalWeight = 0
    val weightedItems =
      items.map { item =>
        val itemWeight = weight(item)
        totalWeight = totalWeight + itemWeight
        totalWeight -> item
      }
    val selection = Random.nextInt(totalWeight)
    weightedItems.find(_._1 > selection).map(_._2)
  }

  /**
    * Performs a cross-join between each of the lists in the `lists` parameter.
    *
    * Examples:
    *
    * permutations(List(List(1,2,3)))
    * -> List(List(1), List(2), List(3))
    *
    * permutations(List(List(1,2,3), List(10,20,30)))
    * -> List(List(1, 10), List(2, 10), List(3, 10), List(1, 20), List(2, 20), List(3, 20), List(1, 30), List(2, 30), List(3, 30))
    */
  def cartesianProduct[V](lists: List[List[V]]): List[List[V]] =
    lists.sequence

  def cartesianProduct[K, V](lists: Map[K, List[V]]): List[Map[K, V]] =
    lists.sequence
}
