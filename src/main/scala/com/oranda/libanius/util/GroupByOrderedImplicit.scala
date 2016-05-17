package com.oranda.libanius.util

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.LinkedHashSet

object CollectionHelpers {
  /*
   * Provide a groupBy function that preserves order of the keys in the original collection.
   *
   * Code adapted from a StackOverflow answer by Leif Wickland:
   *   http://stackoverflow.com/questions/9594431/scala-groupby-preserving-insertion-order
   */
  implicit class GroupByOrderedImplicit[A](t: Traversable[A]) {
    def groupByOrdered[K](f: A => K): LinkedHashMap[K, LinkedHashSet[A]] = {
      val map = LinkedHashMap[K,LinkedHashSet[A]]()
      for (i <- t) {
        val key = f(i)
        map(key) = map.lift(key).getOrElse(LinkedHashSet[A]()) + i
      }
      map
    }
  }

}
