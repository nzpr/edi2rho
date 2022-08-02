package coop.rchain.rabbit2rho.gentran

import scala.collection.compat.immutable.LazyList
import scala.collection.mutable

final case class AMapHelper(tree: mutable.Map[String, Either[AMapHelper, Int]]) {
  // Init map with branching and records
  def init(
      branching: List[(Option[String], String)],
      recordsSchemas: Map[String, Set[String]]
  ): AMapHelper = {
    val withBranches = branching.foldLeft(this) {
      case (acc, (prefOpt, k)) => acc.initBranch(k, prefOpt)
    }
    recordsSchemas.foldLeft(withBranches) {
      case (acc, (prefix, set)) =>
        acc.initLeaf(prefix, set)
    }
  }

  // Initialize child map.
  private def initBranch(k: String, prefix: Option[String]): AMapHelper = {
    val subMap = prefix.flatMap(x => findSubMap(rhoName(x))).getOrElse(this)
    subMap.tree
      .update(rhoName(k), Left(AMapHelper(mutable.Map.empty[String, Either[AMapHelper, Int]])))
    this
  }

  // Initialize values of a leaf map.
  private def initLeaf(mapKey: String, values: Set[String]): AMapHelper = {
    val targetOpt = findSubMap(rhoName(mapKey))
    assert(targetOpt.isDefined)
    val t = targetOpt.get
    values.foreach { k =>
      val rhoK           = rhoName(k)
      val numberOfValues = getLeafValues(None, rhoK).size
      t.tree.update(rhoK, Right(numberOfValues))
    }
    this
  }

  private def findSubMap(key: String): Option[AMapHelper] = subMaps(None).collectFirst {
    case (k, v) if k == key => v
  }

  private def subMaps(root: Option[AMapHelper]): LazyList[(String, AMapHelper)] =
    LazyList
      .unfold(List(root.getOrElse(this).tree)) { trees =>
        val a = trees.flatMap {
          _.collect { case (k, Left(v)) => k -> v }
        }
        if (a.nonEmpty) Some((a, a.map(_._2.tree)))
        else None
      }
      .flatten

  def getLeafData: Map[String, List[Int]] = groupedLeafs(None)

  def getBranchData: List[(String, Map[String, List[Int]])] = subMaps(None).toList.map {
    case (name, subMap) => (name, groupedLeafs(Some((name, subMap))))
  }

  // Find all leaf values in Root. If rootNameOpt = None find in all tree.
  private def getLeafValues(rootNameOpt: Option[String], leafName: String): List[Int] = {
    val targetOpt = rootNameOpt.map { subMapName =>
      val subMapOpt = findSubMap(subMapName)
      assert(subMapOpt.isDefined)
      (subMapName, subMapOpt.get)
    }
    groupedLeafs(targetOpt).getOrElse(leafName, List())
  }

  private def groupedLeafs(rootOpt: Option[(String, AMapHelper)]): Map[String, List[Int]] = {
    def leafs(subMap: AMapHelper): List[(String, Int)] =
      subMap.tree.toList.collect { case (k, Right(v)) => k -> v }

    val subms = subMaps(rootOpt.map(_._2)).toList
    val maps  = rootOpt.map(_ +: subms).getOrElse(subms)
    val ls    = maps.flatMap { case (_, subMap) => leafs(subMap) }
    ls.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
  }

  private def rhoName(name: String): String = name.replace(":", "_colon_")

}

object AMapHelper {
  def apply(
      branching: List[(Option[String], String)],
      records: Map[String, Set[String]]
  ): AMapHelper =
    AMapHelper(mutable.Map.empty[String, Either[AMapHelper, Int]]).init(branching, records)
}
