package strawman
package collection
package immutable

import scala.Ordering

trait SortedMap[K, +V]
  extends Map[K, V]
     with collection.SortedMap[K, V]
     with SortedMapOps[K, V, SortedMap, SortedMap[K, V]]

trait SortedMapOps[K, +V, +CC[X, +Y] <: SortedMap[X, Y] with SortedMapOps[X, Y, CC, _], +C <: SortedMap[K, V]]
  extends MapOps[K, V, Map, C]
     with collection.SortedMapOps[K, V, CC, C] {

  override def + [V1 >: V](kv: (K, V1)): CC[K, V1] = updated(kv._1, kv._2)
  def updated[V1 >: V](key: K, value: V1): CC[K, V1]
}

