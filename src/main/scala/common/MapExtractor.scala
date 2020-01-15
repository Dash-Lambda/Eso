package common

import scala.annotation.tailrec
import scala.collection.immutable

case class MapExtractor[K, V](map: immutable.HashMap[K, V]) {
  def unapplySeq(keys: Seq[K]): Option[List[V]] = {
    @tailrec
    def udo(src: Vector[K], ac: Vector[V] = Vector()): Option[Vector[V]] = src match{
      case k +: ks => map.get(k) match{
        case Some(v) => udo(ks, ac :+ v)
        case None => None}
      case _ => Some(ac)}
    udo(keys.toVector) map (_.toList)}
}