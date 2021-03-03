package co.gyeongmin.lang.functional

trait Monoid[V] {
  def op(a: V, b: V): V

  def zero: V
}

object MonoidSyntax {
  implicit val intMergeMonoid: Monoid[Int] = new Monoid[Int] {
    override def zero: Int = 0
    override def op(a: Int, b: Int): Int = a + b
  }

  implicit def listMergeWithDistinctMonoid[T]: Monoid[List[T]] =
    new Monoid[List[T]] {
      override def op(a: List[T], b: List[T]): List[T] = (a ++ b).distinct
      override def zero: List[T] = List()
    }
}

object Monoid {
  def mergeMap[K, V: Monoid](a: Map[K, V], b: Map[K, V]): Map[K, V] = {
    val mn = implicitly[Monoid[V]]
    val aKey = a.keys.toSet
    val bKey = b.keys.toSet

    val keys = aKey ++ bKey
    keys.foldLeft(Map.empty[K, V]) { (acc, elem) =>
      acc.updated(
        elem,
        mn.op(a.getOrElse(elem, mn.zero), b.getOrElse(elem, mn.zero))
      )
    }
  }
}
