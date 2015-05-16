object AmortizedQueue {
  def apply[A](elts: A*): AmortizedQueue[A] = AmortizedQueue(elts.toList, Nil)
}

case class AmortizedQueue[+A](front: List[A], rear: List[A]) {

  def head(): A = front.head

  def tail(): AmortizedQueue[A] = front match {
    case h :: Nil => AmortizedQueue(rear.reverse, Nil)
    case h :: t => AmortizedQueue(t, rear)
  }

  def snoc[B >: A](b: B): AmortizedQueue[B] = front match {
    case Nil => AmortizedQueue(List(b), Nil)
    case _ => AmortizedQueue(front, b :: rear)
  }
}
