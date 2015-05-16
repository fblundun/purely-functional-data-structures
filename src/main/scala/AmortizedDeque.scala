object AmortizedDeque {
  def apply[A](elts: A*): AmortizedDeque[A] = elts.toList match {
    case Nil => AmortizedDeque(Nil, Nil)
    case h :: t => AmortizedDeque(List(h), t)
  }
}

case class AmortizedDeque[A](front: List[A], rear: List[A]) {
  def head(): A = front match {
    case h :: t => h
    case Nil => rear.head
  }

  def last(): A = rear match {
    case h :: t => h
    case Nil => front.head
  }

  def cons[B >: A](b: B): AmortizedDeque[B] = this match {
    case AmortizedDeque(h :: Nil, Nil) => AmortizedDeque(List(b), List(h))
    case _ => AmortizedDeque(b :: front, rear)
  }

  def snoc[B >: A](b: B): AmortizedDeque[B] = this match {
    case AmortizedDeque(Nil, h :: Nil) => AmortizedDeque(List(h), List(b))
    case _ => AmortizedDeque(front, b :: rear)
  }

  def tail(): AmortizedDeque[A] = this match {
    case AmortizedDeque(Nil, h :: Nil) => AmortizedDeque(Nil, Nil)
    case AmortizedDeque(h :: Nil, _) => split(rear)
    case _ => AmortizedDeque(front.tail, rear)
  }

  def init(): AmortizedDeque[A] = this match {
    case AmortizedDeque(h :: Nil, Nil) => AmortizedDeque(Nil, Nil)
    case AmortizedDeque(_, h :: Nil) => split(front)
    case _ => AmortizedDeque(front, rear.tail)
  }

  private def split(elts: List[A]): AmortizedDeque[A] = {
    val splitPoint = elts.size / 2
    AmortizedDeque(elts.take(splitPoint), elts.drop(splitPoint))
  }

}
