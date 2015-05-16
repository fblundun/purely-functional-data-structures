object UnbalancedTree {
  def apply[A: Ordering](elts: A*): UnbalancedTree[A] = {
    import scala.annotation.tailrec
    @tailrec
    def iter(a: List[A], acc: UnbalancedTree[A] = E): UnbalancedTree[A] = a match {
      case Nil => acc
      case h :: t => iter(t, acc.add(h))
    }
    iter(elts.toList)
  }
}

sealed trait UnbalancedTree[+A] {
  def add[B >: A](b: B)(implicit ord: Ordering[B]): UnbalancedTree[B] = {
    import ord.mkOrderingOps
    this match {
      case E => T(E, b, E)
      case T(l, a, r) => if (b < a) {
        T(l.add(b), a, r)
      } else {
        T(l, a, r.add(b))
      }
    }
  }

  def contains[B >: A](b: B)(implicit ord: Ordering[B]): Boolean = {
    import ord.mkOrderingOps
    this match {
      case E => false
      case T(l, a, r) => if (b < a) {
        l.contains(b)
      } else if (b > a) {
        r.contains(b)
      } else {
        true
      }
    }
  }
}

case object E extends UnbalancedTree[Nothing]

case class T[+A](
  left: UnbalancedTree[A],
  value: A,
  right: UnbalancedTree[A]) extends UnbalancedTree[A]
