package coop.rchain.rabbit2rho.gentran

import scala.collection.compat.immutable.LazyList
import scala.collection.mutable

final case class AMap[K, V](tree: mutable.Map[K, Either[AMap[K, V], V]]) {
  // Get value, optionally specifying the scope of search by some inner map
  // Value of the first key found is returned
  def get(key: K, prefix: Option[K] = None): Option[V] = {
    val subMap = prefix.flatMap(findSubMap).getOrElse(this)
    subMap.iterator.collectFirst { case (k, v) if k == key => v }
  }

  // Set value for all instances of a key in all inner maps.
  // Prefix can be specified to limit the scope.
  def set(kv: (K, V), prefix: Option[K] = None): AMap[K, V] = {
    val target =
      prefix.flatMap(p => subMaps.collectFirst { case (k, v) if p == k => v }).getOrElse(this)
    target.setAll(kv)
    this
  }

  // Init map with branching and records
  def init(branching: List[(Option[K], K)], recordsSchemas: Map[K, Map[K, V]]): AMap[K, V] = {
    val withBranches = branching.foldLeft(this) {
      case (acc, (prefOpt, k)) => acc.initBranch(k, prefOpt)
    }
    recordsSchemas.foldLeft(withBranches) {
      case (acc, (prefix, map)) =>
        acc.initLeaf(prefix, map)
    }
  }

  // Initialize child map.
  private def initBranch(k: K, prefix: Option[K]): AMap[K, V] = {
    val subMap = prefix.flatMap(findSubMap).getOrElse(this)
    subMap.tree.update(k, Left(AMap(mutable.Map.empty[K, Either[AMap[K, V], V]])))
    this
  }

  // Initialize values of a leaf map.
  private def initLeaf(mapKey: K, values: Map[K, V]): AMap[K, V] = {
    val targetOpt = findSubMap(mapKey)
    assert(targetOpt.isDefined)
    val t = targetOpt.get
    values.foreach { case (k, v) => t.tree.update(k, Right(v)) }
    this
  }

  private def set1(kv: (K, V)): AMap[K, V] = {
    if (tree.contains(kv._1)) tree.update(kv._1, Right(kv._2))
    this
  }

  private def setAll(kv: (K, V)): AMap[K, V] = {
    subMaps.foreach { case (_, map) => map.set1(kv) }
    this
  }

  private def findSubMap(key: K): Option[AMap[K, V]] = subMaps.collectFirst {
    case (k, v) if k == key => v
  }

  private def subMaps: LazyList[(K, AMap[K, V])] =
    LazyList
      .unfold(List(this.tree)) { trees =>
        val a = trees.flatMap { _.collect { case (k, Left(v)) => k -> v } }
        if (a.nonEmpty) Some((a, a.map(_._2.tree)))
        else None
      }
      .flatten

  private def iterator: Iterator[(K, V)] =
    (LazyList(this) ++ subMaps.map(_._2))
      .flatMap(_.tree.collect { case (k, Right(v)) => k -> v }.toIterator)
      .iterator
}

object AMap {
  def apply[K, V](
      branching: List[(Option[K], K)],
      records: Map[K, Set[K]],
      empty: V
  ): AMap[K, V] = {
    val recordValues = records.mapValues(_.map(_ -> empty).toMap)
    AMap(mutable.Map.empty[K, Either[AMap[K, V], V]]).init(branching, recordValues)
  }
}
